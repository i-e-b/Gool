using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using Gool.Parsers.Terminals;
using Gool.Scanners;

namespace Gool.Results;

/// <summary>
/// Creates and stores parser matches.
/// </summary>
public class ParserMatch
{
    private readonly Func<string, string>? _mutator;

    /// <summary>
    /// Builds a new match from a parser, input, and result range.
    /// </summary>
    /// <param name="source">The parser that made this match. This is required for tags and scopes</param>
    /// <param name="scanner">Scanner for this match. This is used for the final output</param>
    /// <param name="offset">Start of the match</param>
    /// <param name="length">Number of characters in the match</param>
    /// <param name="mutator">Optional function to modify the output of <see cref="Value"/></param>
    public ParserMatch(IParser? source, IScanner scanner, int offset, int length, Func<string,string>? mutator = null)
    {
        _mutator = mutator;
        SourceParser = source;

        Scanner = scanner; // ?? throw new ArgumentNullException(nameof(scanner), "Tried to create a match from a null scanner.");
        Offset = offset;
        Length = length;
    }

    /// <summary>
    /// Create a match from a string, that represents the entire string.
    /// </summary>
    public ParserMatch(string value, string? tag = null)
    {
        SourceParser = new NullParser("Match");
        SourceParser.Tag = tag;

        Scanner = new ScanStrings(value);
        Offset = 0;
        Length = value.Length;
    }

    /// <summary>
    /// List of child matches (should you need the AST)
    /// </summary>
    public readonly MaybeList<ParserMatch> ChildMatches = new();

    /// <summary>
    /// The parser that generated this match
    /// </summary>
    public readonly IParser? SourceParser;

    /// <summary>
    /// Scanner
    /// </summary>
    public readonly IScanner Scanner;

    /// <summary>
    /// Offset
    /// </summary>
    public readonly int Offset;

    /// <summary>
    /// Length
    /// </summary>
    public int Length;

    /// <summary>
    /// Extracts the match value
    /// </summary>
    public string Value
    {
        get
        {
            if (Length < 0) throw new Exception("no match");
            if (_mutator is not null) return _mutator(Scanner.UntransformedSubstring(Offset, Length));
            return Scanner.UntransformedSubstring(Offset, Length);
        }
    }

    /// <summary>
    /// Tag added to the parser that resulted in this match, if any.
    /// </summary>
    public string? Tag => SourceParser?.Tag;
    
    /// <summary>
    /// Get the scope direction of the source parser
    /// <ul>
    /// <li>Positive values open a new scope</li>
    /// <li>Negative values close the current scope</li>
    /// <li>Zero value does not change scope (default)</li>
    /// </ul>
    /// </summary>
    public ScopeType Scope => SourceParser?.Scope ?? ScopeType.None;

    /// <summary>
    /// True if match successful
    /// </summary>
    public bool Success => Length >= 0;

    /// <summary>
    /// True if match empty
    /// </summary>
    public bool Empty => (Length <= 0) && (ChildMatches.Count < 1);

    /// <summary>
    /// Next offset after this match
    /// </summary>
    public int Right => Length > 0 ? Offset + Length : Offset;

    /// <summary>
    /// Return the match value string
    /// </summary>
    public override string ToString()
    {
        if (Success) return Empty ? $"<empty ({Offset})>" : Value;
        return $"<failure ({SourceParser}: {Offset}..{Right})>";
    }

    /// <summary>
    /// Diagnostic string for this match
    /// </summary>
    public string Description()
    {
        if (Success) return $"Offset={Offset}; Length={Length}; Source={(SourceParser?.GetType().Name)??"<null>"}; Value='{Value}';";
        return $"Offset={Offset}; Length={Length}; Source={(SourceParser?.GetType().Name)??"<null>"};";
    }

    /// <summary>
    /// Create a new match by joining a pair of existing matches.
    /// Most of the time this performs a binary join, but some parser
    /// flags can change this (see <see cref="ScopeType"/>)
    /// </summary>
    /// <returns>Match covering and containing both left and right</returns>
    public static ParserMatch Join(IParser source, ParserMatch? left, ParserMatch right)
    {
        if (right == null) throw new NullReferenceException("Can't Join null match");
        if (!right.Success) throw new ArgumentException("Can't Join failure match");

        // Joining success onto failure gives only the success
        if (left?.Success != true)
        {
            if (string.IsNullOrEmpty(source.Tag)) return right;
            var chainResult = new ParserMatch(source, right.Scanner, right.Offset, right.Length);
            if (!right.Empty) chainResult.ChildMatches.Add(right);
            return chainResult;
        }
        
        if (left.Scanner != right.Scanner) throw new ArgumentException("Can't Join between different scanners");

        // Reduce overlapping matches, if it doesn't loose information
        if (left.Contains(right) && NoMeta(left, right))
        {
            var leftOnlyResult = new ParserMatch(source, left.Scanner, left.Offset, left.Length);
            if (!left.Empty) leftOnlyResult.ChildMatches.Add(left);
            return leftOnlyResult;
        }
        if (right.Contains(left) && NoMeta(left, right))
        {
            var rightOnlyResult = new ParserMatch(source, right.Scanner, right.Offset, right.Length);
            if (!right.Empty) rightOnlyResult.ChildMatches.Add(right);
            return rightOnlyResult;
        }
        
        
        var length = right.Right - left.Offset;
        var joinResult = new ParserMatch(source, left.Scanner, left.Offset, length);
        
        
        // If one of the parsers is a 'pivot' scope, and the other isn't
        // then we should re-arrange the output so the pivot is the parent
        // and non-pivot are children

        if (IsValidPivot(left, right))
        {
            if (left.Scope == ScopeType.Pivot)
            {
                left.ChildMatches.Add(right);
                joinResult.ChildMatches.Add(left);
                return joinResult;
            }
            
            if (right.Scope == ScopeType.Pivot)
            {
                right.ChildMatches.Add(left);
                joinResult.ChildMatches.Add(right);
                return joinResult;
            }
        }

        // Normal join between left and right
        if (!left.Empty) joinResult.ChildMatches.Add(left);
        if (!right.Empty) joinResult.ChildMatches.Add(right);
        return joinResult;
    }

    private static bool NoMeta(ParserMatch left, ParserMatch right)
    {
        return (!left.HasMetaData()) && (!right.HasMetaData());
    }

    /// <summary>
    /// If one of the parsers is a pivot, but not both
    /// </summary>
    private static bool IsValidPivot(ParserMatch left, ParserMatch right)
    {
        if (left.Scope == ScopeType.Pivot && right.Scope == ScopeType.Pivot) return false;
        return left.Scope == ScopeType.Pivot || right.Scope == ScopeType.Pivot;
    }

    /// <summary>
    /// return <c>true</c> if this match entirely contains the other
    /// </summary>
    private bool Contains(ParserMatch other)
    {
        return (Offset <= other.Offset) && (Right >= other.Right);
    }
    
    /// <summary>
    /// Walk *every* match in this match tree. This will usually result in
    /// duplicate matches.
    /// </summary>
    public IEnumerable<ParserMatch> DepthFirstWalk()
    {
        return DepthFirstWalk(this, _ => true);
    }

    /// <summary>
    /// Return the bottom-most parser matches, including matches
    /// that aren't atoms, ignoring sub-atomic matches and ignoring this match
    /// even if this is atomic.
    /// </summary>
    public IEnumerable<ParserMatch> BottomLevelMatchesDepthFirst()
    {
        return DepthFirstWalk(this, node => node.ChildMatches.Count < 1);
    }
    
    /// <summary>
    /// Return the bottom-most parser matches, including matches
    /// that aren't atoms, ignoring sub-atomic matches and ignoring this match
    /// even if this is atomic.
    /// </summary>
    public IEnumerable<ParserMatch> BottomLevelMatchesBreadthFirst()
    {
        return BreadthFirstWalk(this, node => node.ChildMatches.Count < 1);
    }

    /// <summary>
    /// Return all parser matches where the parser has been given a tag value.
    /// This can be used to convert the parser token results into a meaningful structure.
    /// </summary>
    public IEnumerable<ParserMatch> TaggedTokensDepthFirst()
    {
        return DepthFirstWalk(this, m => m.Tag is not null);
    }
    
    /// <summary>
    /// Return all parser matches where the parser has been given a tag value.
    /// This can be used to convert the parser token results into a meaningful structure.
    /// </summary>
    public IEnumerable<ParserMatch> TaggedTokensBreadthFirst()
    {
        return BreadthFirstWalk(this, m => m.Tag is not null);
    }
    
    /// <summary>
    /// Return all parser matches where the parser has the exact requested tag value
    /// </summary>
    public IEnumerable<ParserMatch> ChildrenWithTag(string tagValue)
    {
        return DepthFirstWalk(this, m => m.Tag == tagValue);
    }

    /// <summary>
    /// Does a recursive, depth-first search of this match and all children.
    /// Returns matches where <paramref name="select"/> returns <c>true</c>
    /// </summary>
    public static IEnumerable<ParserMatch> DepthFirstWalk(ParserMatch? node, Func<ParserMatch, bool> select)
    {
        if (node is null) yield break;
        
        if (select(node)) yield return node; // this match

        var check = (ParserMatch n) => DepthFirstWalk(n, select);
        foreach (var m in node.ChildMatches.SelectMany(check))
        {
            if (select(m)) yield return m;
        }
    }
    
    /// <summary>
    /// Does a recursive, breadth-first search of this match and all children.
    /// Returns matches where <paramref name="select"/> returns <c>true</c>
    /// </summary>
    public static IEnumerable<ParserMatch> BreadthFirstWalk(ParserMatch? root, Func<ParserMatch, bool> select)
    {
        if (root is null) yield break;
        
        if (select(root)) yield return root; // this match
        
        var nextSet = new Queue<ParserMatch>(root.ChildMatches);

        while (nextSet.Count > 0)
        {
            var node = nextSet.Dequeue();
            if (select(node)) yield return node; // this match
            
            foreach (var child in node.ChildMatches) nextSet.Enqueue(child);
        }
    }

    /// <summary>
    /// Extend this match's length so it reaches the given offset.
    /// This should only by used by the scanner during auto-advance.
    /// </summary>
    internal void ExtendTo(int offset)
    {
        var newLength = offset - Offset;
        if (newLength > Length) Length = newLength;
    }

    /// <summary>
    /// Check if this match has the same offset and length
    /// </summary>
    public bool SameAs(ParserMatch? previousMatch)
    {
        if (previousMatch is null) return false;

        return previousMatch.Offset == Offset && previousMatch.Length == Length;
    }

    /// <summary>
    /// This match is being passed through a composite,
    /// and may need to collect a tag.
    /// </summary>
    public ParserMatch Through(IParser source)
    {
        // If the parser doesn't add any meta-data, skip joining
        if (!source.HasMetaData()) return this;
        
        // Make a match covering this one, with the new source
        var joinMatch = new ParserMatch(source, Scanner, Offset, Length);
        
        // Join this match to the result if we have any metadata to carry
        if (AnyMetaInTree()) joinMatch.ChildMatches.Add(this);

        return joinMatch;

    }

    private bool AnyMetaInTree()
    {
        if (SourceParser?.HasMetaData() == true) return true;
        return ChildMatches.Any(child => child.AnyMetaInTree());
    }

    /// <summary>
    /// Find and return the first match with the given tag.
    /// Returns <c>null</c> if none found.
    /// </summary>
    public ParserMatch? FindTag(string tag)
    {
        return DepthFirstWalk(this, _ => true).FirstOrDefault(m=>m.Tag == tag);
    }

    /// <summary>
    /// Placeholder for an invalid match
    /// </summary>
    public static ParserMatch NullMatch()
    {
        return new ParserMatch(null, new NullScanner(), 0, -1);
    }

    /// <summary>
    /// Returns true if the source parser has meta data. False otherwise
    /// </summary>
    public bool HasMetaData() => SourceParser?.HasMetaData() == true;
}

/// <summary>
/// Reduced allocation list
/// </summary>
public class MaybeList<T> : IList<T>
{
    private IList<T> _src = Array.Empty<T>();

    /// <inheritdoc />
    public IEnumerator<T> GetEnumerator() => _src.GetEnumerator();

    /// <inheritdoc />
    IEnumerator IEnumerable.GetEnumerator() => ((IEnumerable)_src).GetEnumerator();

    /// <inheritdoc />
    public void Add(T item)
    {
        if (_src is List<T> list) list.Add(item);
        else _src = new List<T>{item};
    }

    /// <inheritdoc />
    public void Clear()
    {
        if (_src is List<T> lst) lst.Clear();
    }

    /// <inheritdoc />
    public bool Contains(T item) => _src.Contains(item);

    /// <inheritdoc />
    public void CopyTo(T[] array, int arrayIndex) => _src.CopyTo(array, arrayIndex);

    /// <inheritdoc />
    public bool Remove(T item)
    {
        if (_src is List<T> lst) return lst.Remove(item);
        return false;
    }

    /// <inheritdoc />
    public int Count => _src.Count;

    /// <inheritdoc />
    public bool IsReadOnly => false;

    /// <inheritdoc />
    public int IndexOf(T item) => _src.IndexOf(item);

    /// <inheritdoc />
    public void Insert(int index, T item) => _src.Insert(index, item);

    /// <inheritdoc />
    public void RemoveAt(int index)
    {
        if (_src is List<T> lst) lst.RemoveAt(index);
    }

    /// <inheritdoc />
    public T this[int index]
    {
        get => _src[index];
        set => _src[index] = value;
    }
}
