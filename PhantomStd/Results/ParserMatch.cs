using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using Phantom.Parsers;

namespace Phantom.Results;

/// <summary>
/// Creates and stores parser matches.
/// </summary>
public class ParserMatch
{
    /// <summary>
    /// Builds a new match
    /// </summary>
    public ParserMatch(IParser? source, IScanner scanner, int offset, int length)
    {
        SourceParser = source;

        Scanner = scanner ?? throw new ArgumentNullException(nameof(scanner), "Tried to create a match from a null scanner.");
        Offset = offset;
        Length = length;
    }

    /// <summary>
    /// List of child matches (should you need the AST)
    /// </summary>
    public List<ParserMatch> ChildMatches { get; } = new();

    /// <summary>
    /// The parser that generated this match
    /// </summary>
    public IParser? SourceParser { get; }

    /// <summary>
    /// Scanner
    /// </summary>
    public IScanner Scanner { get; }

    /// <summary>
    /// Offset
    /// </summary>
    public int Offset { get; private set; }

    /// <summary>
    /// Length
    /// </summary>
    public int Length { get; private set; }

    /// <summary>
    /// Extracts the match value
    /// </summary>
    public string Value
    {
        get
        {
            if (Length < 0) throw new Exception("no match");
            return Scanner.Substring(Offset, Length);
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
    public int ScopeSign => SourceParser?.ScopeSign ?? 0;

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
    /// Return child matches
    /// </summary>
    /// <returns></returns>
    public IEnumerator GetEnumerator()
    {
        foreach (var childMatch in ChildMatches)
        {
            yield return childMatch;
        }
    }

    /// <summary>
    /// Return the match value string
    /// </summary>
    public override string ToString()
    {
        if (Success) return Empty ? "<empty>" : Value;
        return "<failure>";
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
    /// Create a new match by joining a pair of existing matches
    /// </summary>
    /// <returns>Match covering and containing both left and right</returns>
    public static ParserMatch Join(Parser source, ParserMatch left, ParserMatch right)
    {
        if (left == null || right == null) throw new NullReferenceException("Can't concatenate null match");
        if (!right.Success) throw new ArgumentException("Can't concatenate failure match");
        if (left.Scanner != right.Scanner) throw new ArgumentException("Can't concatenate between different scanners");

        //Console.WriteLine($"    J({source.GetTag()}, {left.Tag}, {right.Tag})");
        
        // Joining success onto failure just gives the success
        if (!left.Success)
        {
            if (string.IsNullOrEmpty(source.Tag)) return right;
            var chainResult = new ParserMatch(source, right.Scanner, right.Offset, right.Length);
            if (!right.Empty) chainResult.ChildMatches.Add(right);
            return chainResult;
        }

        // Reduce overlapping matches
        if (left.Contains(right))
        {
            var leftOnlyResult = new ParserMatch(source, left.Scanner, left.Offset, left.Length);
            /*if (!left.Empty)*/ leftOnlyResult.ChildMatches.Add(left);
            return leftOnlyResult;
        }
        if (right.Contains(left))
        {
            var rightOnlyResult = new ParserMatch(source, right.Scanner, right.Offset, right.Length);
            /*if (!right.Empty)*/ rightOnlyResult.ChildMatches.Add(right);
            return rightOnlyResult;
        }
        
        // Normal join between left and right
        var length = right.Right - left.Offset;
        var joinResult = new ParserMatch(source, left.Scanner, left.Offset, length);
        if (!left.Empty) joinResult.ChildMatches.Add(left);
        if (!right.Empty) joinResult.ChildMatches.Add(right);
        return joinResult;
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
    public IEnumerable<ParserMatch> BottomLevelMatches()
    {
        return DepthFirstWalk(this, node => node.ChildMatches.Count < 1);
    }

    /// <summary>
    /// Return all parser matches where the parser has been given a tag value.
    /// This can be used to convert the parser token results into a meaningful structure.
    /// </summary>
    public IEnumerable<ParserMatch> TaggedTokens()
    {
        return DepthFirstWalk(this, m => m.Tag is not null);
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
    private static IEnumerable<ParserMatch> DepthFirstWalk(ParserMatch? node, Func<ParserMatch, bool> select)
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
    /// Return all parser matches where the parser has been given a tag value.
    /// Matches that have a non-zero 'scope' value will build the hierarchy.
    /// </summary>
    public ScopeNode ToScopes()
    {
        var root = ScopeNode.RootNode();
        
        var points = DepthFirstWalk(this, m => m.Tag is not null || m.ScopeSign != 0);

        var cursor = (ScopeNode?)root;
        foreach (var match in points)
        {
            if (cursor is null) break; // this will happen if there are too many scope closes

            switch (match.ScopeSign)
            {
                case > 0:
                    cursor = cursor.OpenScope(match);
                    break;
                case < 0:
                    cursor = cursor.CloseScope(match);
                    break;
                default:
                    cursor.AddDataFrom(match);
                    break;
            }
        }

        return root;
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
        if (SourceParser?.HasMetaData() == true) joinMatch.ChildMatches.Add(this);

        return joinMatch;

    }
}