using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using Gool.Parsers;
using Gool.Parsers.Terminals;
using Gool.Scanners;

namespace Gool.Results;

/// <summary>
/// Creates and stores parser matches, and their child matches.
/// </summary>
/// <remarks>
/// This only supports up to 2 child matches
/// </remarks>
public class ParserMatch
{
    /// <summary> First child match, if any </summary>
    private ParserMatch? _leftChild;

    /// <summary> Second child match, if two children </summary>
    private ParserMatch? _rightChild;

    /// <summary>
    /// Previous sibling parser match, if any.
    /// Will always be <c>null</c> for first match.
    /// </summary>
    public readonly ParserMatch? Previous;

    /// <summary>
    /// Next sibling parser match, if any.
    /// Will always be <c>null</c> for last match.
    /// </summary>
    public ParserMatch? Next;

    /// <summary>
    /// Parent match, if any.
    /// Will always be <c>null</c> for root match.
    /// </summary>
    public ParserMatch? Parent;

    /// <summary>
    /// Builds a new match from a parser, input, and result range.
    /// </summary>
    /// <param name="source">The parser that made this match. This is required for tags and scopes</param>
    /// <param name="scanner">Scanner for this match. This is used for the final output</param>
    /// <param name="offset">Start of the match</param>
    /// <param name="length">Number of characters in the match</param>
    /// <param name="previous">Parser match before this one, to be part of prev/next chain</param>
    public ParserMatch(IParser source, IScanner scanner, int offset, int length, ParserMatch? previous)
    {
        SourceParser = source;
        Scanner = scanner;

        Offset = offset;
        Length = length;
        Success = Length >= 0;
        Right = length > 0 ? offset + length : offset;
        Previous = previous;

        if (previous is not null && (previous.Next is null || previous.Next.Length < 0))
        {
            previous.Next = this;
        }
    }

    /// <summary>
    /// Create a match from a string, that represents that entire string.
    /// This is used to represent transformed values in <see cref="TreeNode{T}"/>s.
    /// </summary>
    public ParserMatch(string value, string? tag = null)
    {
        SourceParser = new NullParser("Match");
        SourceParser.Tag = tag;

        Scanner = new ScanStrings(value);
        Offset = 0;
        Length = value.Length;
        Success = Length >= 0;
        Right = Offset + Length;
    }

    /// <summary>
    /// List of child matches (for traversing the syntax tree)
    /// </summary>
    public IEnumerable<ParserMatch> Children()
    {
        if (_leftChild is not null) yield return _leftChild;
        if (_rightChild is not null) yield return _rightChild;
    }

    /// <summary>
    /// Add a child match. There are a maximum of two children per match.
    /// Each child may have its own children.
    /// </summary>
    [SuppressMessage("ReSharper", "InvocationIsSkipped")]
    private void AddChild(ParserMatch child)
    {
        child.Parent = this;
        if (_leftChild is null) _leftChild = child;
        else if (_rightChild is null) _rightChild = child;
        else Debug.Assert(false, "Too many children");
    }

    /// <summary>
    /// Returns true if this node has child nodes
    /// </summary>
    public bool HasChildren => _leftChild is not null; // left should always be populated first

    /// <summary>
    /// The parser that generated this match
    /// </summary>
    public IParser SourceParser;

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
    /// Next offset after this match
    /// </summary>
    public int Right;

    /// <summary>
    /// Extracts the match value
    /// </summary>
    public string Value => Length < 0 ? "" : Scanner.UntransformedSubstring(Offset, Length);

    /// <summary>
    /// Tag added to the parser that resulted in this match, if any.
    /// </summary>
    public string? Tag => SourceParser.Tag;

    /// <summary>
    /// Get the scope direction of the source parser
    /// <ul>
    /// <li>Positive values open a new scope</li>
    /// <li>Negative values close the current scope</li>
    /// <li>Zero value does not change scope (default)</li>
    /// </ul>
    /// </summary>
    public ScopeType Scope => SourceParser.Scope;

    /// <summary>
    /// True if match successful
    /// </summary>
    public bool Success;

    /// <summary>
    /// True if match empty
    /// </summary>
    public bool Empty => (Length <= 0) && (!HasChildren);

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
        if (Success) return $"Offset={Offset}; Length={Length}; Source={SourceParser.GetType().Name}; Value='{Value}';";
        return $"Offset={Offset}; Length={Length}; Source={SourceParser.GetType().Name};";
    }

    /// <summary>
    /// Create a new match by joining a pair of existing matches.
    /// Most of the time this performs a binary join, but some parser
    /// flags can change this (see <see cref="ScopeType"/>)
    /// </summary>
    /// <returns>Match covering and containing both left and right</returns>
    public ParserMatch Join(ParserMatch? previous, IParser source, ParserMatch right)
    {
        return Join(previous, source, this, right);
    }

    /// <summary>
    /// Create a new match by joining a pair of existing matches.
    /// Most of the time this performs a binary join, but some parser
    /// flags can change this (see <see cref="ScopeType"/>).
    /// <p/>
    /// In some cases, this will merge the two results. If that is not desired, see <see cref="Pair"/>.
    /// </summary>
    /// <returns>Match covering both left and right</returns>
    public static ParserMatch Join(ParserMatch? previous, IParser source, ParserMatch? left, ParserMatch right)
    {
        if (right == null) throw new NullReferenceException("Can't Join null match");
        if (!right.Success) throw new ArgumentException("Can't Join failure match");

        // Joining success onto failure gives only the success
        if (left?.Success != true)
        {
            if (string.IsNullOrEmpty(source.Tag)) return right;
            var chainResult = new ParserMatch(source, right.Scanner, right.Offset, right.Length, previous);
            if (!right.Empty) chainResult.AddChild(right);
            return chainResult;
        }
        
        if (left.Scanner != right.Scanner) throw new ArgumentException("Can't Join between different scanners");

        // Reduce overlapping matches, if it doesn't loose information
        if ((left.Contains(right) && NoMeta(left, right)) || right.Empty)
        {
            var leftOnlyResult = new ParserMatch(source, left.Scanner, left.Offset, left.Length, previous);
            if (!left.Empty) leftOnlyResult.AddChild(left);
            return leftOnlyResult;
        }
        if ((right.Contains(left) && NoMeta(left, right)) || left.Empty)
        {
            var rightOnlyResult = new ParserMatch(source, right.Scanner, right.Offset, right.Length, previous);
            if (!right.Empty) rightOnlyResult.AddChild(right);
            return rightOnlyResult;
        }
        
        
        var length = right.Right - left.Offset;
        var joinResult = new ParserMatch(source, left.Scanner, left.Offset, length, previous);
        
        
        // If one of the parsers is a 'pivot' scope, and the other isn't
        // then we should re-arrange the output so the pivot is the parent
        // and non-pivot are children

        if ((left.Scope == ScopeType.Pivot) ^ (right.Scope == ScopeType.Pivot))
        {
            if (left.Scope == ScopeType.Pivot)
            {
                left.AddChild(right);
                joinResult.AddChild(left);
                return joinResult;
            }
            
            if (right.Scope == ScopeType.Pivot)
            {
                right.AddChild(left);
                joinResult.AddChild(right);
                return joinResult;
            }
        }

        // Normal join between left and right
        if (!left.Empty) joinResult.AddChild(left);
        if (!right.Empty) joinResult.AddChild(right);
        return joinResult;
    }

    /// <summary>
    /// Create a match from two children.
    /// These are never merged, unlike <see cref="Join(ParserMatch?, IParser, ParserMatch?, ParserMatch)"/>
    /// </summary>
    /// <returns>Match containing both left and right</returns>
    public static ParserMatch Pair(ParserMatch? previous, IParser source, ParserMatch left, ParserMatch right)
    {
        var length     = right.Right - left.Offset;
        var joinResult = new ParserMatch(source, left.Scanner, left.Offset, length, previous);
        joinResult.AddChild(left);
        joinResult.AddChild(right);
        return joinResult;
    }

    private static bool NoMeta(ParserMatch left, ParserMatch right)
    {
        return (!left.HasMetaData()) && (!right.HasMetaData());
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
        return DepthFirstWalk(this, node => !node.HasChildren);
    }
    
    /// <summary>
    /// Return the bottom-most parser matches, including matches
    /// that aren't atoms, ignoring sub-atomic matches and ignoring this match
    /// even if this is atomic.
    /// </summary>
    public IEnumerable<ParserMatch> BottomLevelMatchesBreadthFirst()
    {
        return BreadthFirstWalk(this, node => !node.HasChildren);
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
        foreach (var m in node.Children().SelectMany(check))
        {
            if (select(m)) yield return m;
        }
    }
    
    /// <summary>
    /// Does a recursive, breadth-first search of this match and all children.
    /// Returns matches where <paramref name="select"/> returns <c>true</c>
    /// </summary>
    // ReSharper disable once MemberCanBePrivate.Global
    public static IEnumerable<ParserMatch> BreadthFirstWalk(ParserMatch? root, Func<ParserMatch, bool> select)
    {
        if (root is null) yield break;
        
        if (select(root)) yield return root; // this match
        
        var nextSet = new Queue<ParserMatch>(root.Children());

        while (nextSet.Count > 0)
        {
            var node = nextSet.Dequeue();
            if (select(node)) yield return node; // this match
            
            foreach (var child in node.Children()) nextSet.Enqueue(child);
        }
    }

    /// <summary>
    /// Extend this match's length so it reaches the given offset.
    /// This should only by used by the scanner during auto-advance.
    /// </summary>
    internal void ExtendTo(int offset)
    {
        var newLength = offset - Offset;
        if (newLength > Length)
        {
            Length = newLength;
            Success = Length >= 0;
            Right = Offset + Length;
        }
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
    public ParserMatch Through(IParser source, ParserMatch? previous)
    {
        // In case of over-enthusiastic .Through()
        if (source == SourceParser) return this;

        // If the parser doesn't add any meta-data, skip joining
        if (!source.HasMetaData()) return this;

        // If the existing match doesn't have any meta, just copy ourselves in
        if (!HasMetaData())
        {
            SourceParser = source;
            return this;
        }

        // Make a match covering this one, with the new source
        var joinMatch = new ParserMatch(source, Scanner, Offset, Length, previous);
        
        // Join this match to the result if we have any metadata to carry
        if (AnyMetaInTree()) joinMatch.AddChild(this);

        return joinMatch;

    }

    private bool AnyMetaInTree()
    {
        if (SourceParser.HasMetaData()) return true;
        return Children().Any(child => child.AnyMetaInTree());
    }

    /// <summary>
    /// Find and return the first match with the given tag.
    /// Returns <c>null</c> if none found.
    /// </summary>
    public ParserMatch? GetTag(string tag)
    {
        return DepthFirstWalk(this, _ => true).FirstOrDefault(m=>m.Tag == tag);
    }

    /// <summary>
    /// Find and return all matches with the given tag.
    /// Returns empty if none found.
    /// </summary>
    public IEnumerable<ParserMatch> FindByTag(string tag)
    {
        return DepthFirstWalk(this, _ => true).Where(m=>m.Tag == tag);
    }

    /// <summary>
    /// Placeholder for an invalid match
    /// </summary>
    public static ParserMatch NullMatch(string source)
    {
        return new ParserMatch(new NullParser(source), new NullScanner(), 0, -1, null);
    }

    /// <summary>
    /// Returns true if the source parser has meta data. False otherwise
    /// </summary>
    public bool HasMetaData() => SourceParser.HasMetaData();

    /// <summary>
    /// Return this match with no child matches
    /// </summary>
    public ParserMatch Compact()
    {
        return new ParserMatch(SourceParser, Scanner, Offset, Length, Previous);
    }

    /// <summary>
    /// Create a copy of this parser match, with a new source parser (used for Wrapper)
    /// </summary>
    internal ParserMatch ReSource(IParser newSource)
    {
        return new ParserMatch(newSource, Scanner, Offset, Length, Previous) {
            _leftChild = _leftChild,
            _rightChild = _rightChild
        };
    }
}
