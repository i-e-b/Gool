using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;

namespace Phantom.Parsers;

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
    public string? Tag => SourceParser?.GetTag();

    /// <summary>
    /// True if match successful
    /// </summary>
    public bool Success => Length >= 0;

    /// <summary>
    /// True if match empty
    /// </summary>
    public bool Empty => Length <= 0;

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
        return Success ? Value : "";
    }

    /// <summary>
    /// Diagnostic string for this match
    /// </summary>
    public string Description()
    {
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

        if (!left.Success) return right; // Joining success onto failure just gives the success
        
        /*
        // if one is a super-set of the other, only return the larger
        if (left.Contains(right) && right.ChildMatches.Count < 1) return left;
        if (right.Contains(left) && left.ChildMatches.Count < 1) return right;

        var result = new ParserMatch(source, left.Scanner, left.Offset, right.Right);

        // Add child matches if they have tags in their tree
        if (!string.IsNullOrEmpty(left.Tag)) result.ChildMatches.Add(left);
        if (!string.IsNullOrEmpty(right.Tag)) result.ChildMatches.Add(right);

        return result;*/
        
        var result = new ParserMatch(source, left.Scanner, left.Offset, right.Right);
        result.ChildMatches.Add(left);
        result.ChildMatches.Add(right);
        return result;
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
        return DepthFirstWalk(this);
    }

    private static IEnumerable<ParserMatch> DepthFirstWalk(ParserMatch? node)
    {
        if (node is null) yield break;
        yield return node; // this match
        foreach (var child in node.ChildMatches)
        {
            foreach (var m in DepthFirstWalk(child)) yield return m;
        }
    }


    /// <summary>
    /// Return the bottom-most parser matches, including matches
    /// that aren't atoms, ignoring sub-atomic matches and ignoring this match
    /// even if this is atomic.
    /// </summary>
    public IEnumerable<ParserMatch> BottomLevelMatches()
    {
        foreach (ParserMatch m in this)
        {
            foreach (var m2 in BottomLevelMatches(m)) yield return m2;
        }
    }

    private static IEnumerable<ParserMatch> BottomLevelMatches(ParserMatch? node)
    {
        if (node == null || node.Empty) yield break;

        if (node.ChildMatches.Count < 1)
        {
            yield return node; // no children, so yield self (this is the bottom level)
            yield break;
        }

        foreach (var m in node.ChildMatches.SelectMany(BottomLevelMatches))
        {
            yield return m;
        }
    }

    /// <summary>
    /// Return all parser matches where the parser has been given a tag value.
    /// This can be used to convert the parser token results into a meaningful structure.
    /// </summary>
    public IEnumerable<ParserMatch> TaggedTokens()
    {
        return TaggedTokensWalk(this);
    }

    private static IEnumerable<ParserMatch> TaggedTokensWalk(ParserMatch? node)
    {
        if (node is null) yield break;

        if (node.Tag is not null) yield return node; // this match

        foreach (var child in node.ChildMatches)
        {
            foreach (var m in DepthFirstWalk(child))
            {
                if (m.Tag is not null) yield return m;
            }
        }
    }

    /// <summary>
    /// Return all parser matches where the parser has the exact requested tag value
    /// </summary>
    public IEnumerable<ParserMatch> ChildrenWithTag(string tagValue)
    {
        return TaggedTokensWalk(this).Where(token => token.Tag == tagValue);
    }

    /// <summary>
    /// Extend this match's length so it reaches the given offset
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
}