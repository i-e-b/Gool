using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;

namespace Phantom.Parsers;

/// <summary>
/// Creates and stores parser matches.
/// </summary>
public class ParserMatch : IEnumerable<ParserMatch>
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
    public IParser? SourceParser { get; private set; }

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

    #region IEnumerable Members

    /// <inheritdoc />
    public IEnumerator GetEnumerator()
    {
        foreach (var childMatch in ChildMatches)
        {
            yield return childMatch;
        }
    }

    IEnumerator<ParserMatch> IEnumerable<ParserMatch>.GetEnumerator()
    {
        foreach (var childMatch in ChildMatches)
        {
            yield return childMatch;
        }
    }

    #endregion

    /// <summary>
    /// Return the match value string
    /// </summary>
    public override string ToString()
    {
        return Value;
    }

    /// <summary>
    /// Create a new match by joining a pair of existing matches
    /// </summary>
    /// <returns>Match covering and containing both left and right</returns>
    public static ParserMatch Concat(Parser source, ParserMatch left, ParserMatch right)
    {
        if (left == null || right == null)
            throw new NullReferenceException("Can't concatenate null match");
        if (!left.Success || !right.Success)
            throw new ArgumentException("Can't concatenate to failure match");
        if (left.Scanner != right.Scanner)
            throw new ArgumentException("Can't concatenate between different scanners");

        var m = new ParserMatch(source, left.Scanner, left.Offset, left.Length);
        m.AddSubMatch(left);
        m.AddSubMatch(right);

        return m;
    }

    /// <summary>
    /// Add a sub-parser match, and include it's coverage in this match's coverage.
    /// </summary>
    /// <param name="m">Match to add</param>
    public void AddSubMatch(ParserMatch m)
    {
        if (m == null)
            throw new ArgumentNullException(nameof(m), "Can't add null match.");
        if (!m.Success)
            throw new ArgumentException("Can't add failure match.");

        // Add the child
        ChildMatches.Add(m);

        // extend coverage if needed.
        if (m.Empty)
            return;

        int offset, length;

        if (Empty)
        {
            offset = m.Offset;
            length = m.Length;
        }
        else
        {
            offset = Math.Min(Offset, m.Offset);
            int mEnd = m.Offset + m.Length;
            int tEnd = Offset + Length;
            int end = Math.Max(mEnd, tEnd);
            length = end - offset;
        }

        Offset = offset;
        Length = length;
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
}