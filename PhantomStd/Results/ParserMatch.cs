using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using Gool.Parsers;
using Gool.Parsers.Terminals;
using Gool.Scanners;

namespace Gool.Results;

/// <summary>
/// A <see cref="ParserMatch"/> with additional type information from the source parser
/// </summary>
/// <typeparam name="T">Type of the source parser</typeparam>
public class ParserMatch<T> : ParserMatch
{
    /// <summary>
    /// Parser that found this match
    /// </summary>
    public readonly T Parser;

    /// <summary>
    /// test
    /// </summary>
    public ParserMatch(ParserMatch m)
    {
        Parser = (T)m.SourceParser;
        SrcLeftChild = m.LeftChild;
        SrcRightChild = m.RightChild;
        Previous = m.Previous;
        SourceParser = m.SourceParser;
        Scanner = m.Scanner;
        Offset = m.Offset;
        Length = m.Length;
        Right = m.Right;
        Success = m.Success;
    }
}

/// <summary>
/// Creates and stores parser matches, and their child matches.
/// </summary>
/// <remarks>
/// This only supports up to 2 child matches
/// </remarks>
public class ParserMatch
{
    #region Data

    /// <summary> First child match, if any </summary>
    protected ParserMatch? SrcLeftChild;

    /// <summary> Second child match, if two children </summary>
    protected ParserMatch? SrcRightChild;

    /// <summary>
    /// Previous sibling parser match, if any.
    /// Will always be <c>null</c> for first match.
    /// </summary>
    public ParserMatch? Previous;

    /// <summary>
    /// The parser that generated this match
    /// </summary>
    public IParser SourceParser;

    /// <summary>
    /// Scanner
    /// </summary>
    public IScanner Scanner;

    /// <summary>
    /// Offset
    /// </summary>
    public int Offset;

    /// <summary>
    /// Length
    /// </summary>
    public int Length;

    /// <summary>
    /// Next offset after this match
    /// </summary>
    public int Right;

    /// <summary>
    /// True if match successful
    /// </summary>
    public bool Success;

    #endregion Data

    #region Computed properties

    /// <summary>
    /// Returns true if this node has child nodes
    /// </summary>
    public bool HasChildren => SrcLeftChild is not null; // left should always be populated first

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
    /// True if match empty
    /// </summary>
    public bool Empty => (Length <= 0) && (!HasChildren);

    /// <summary>
    /// The left-side child match, if any
    /// </summary>
    public ParserMatch? LeftChild => SrcLeftChild;

    /// <summary>
    /// The right-side child match, if any
    /// </summary>
    public ParserMatch? RightChild => SrcRightChild;

    #endregion Computed properties

    /// <summary>
    /// Internal only blank constructor
    /// </summary>
    public ParserMatch()
    {
        SourceParser = new NullParser("Internal");
        Scanner = new NullScanner();
    }

    /// <summary>
    /// Builds a new match from a parser, input, and result range.
    /// </summary>
    /// <param name="source">The parser that made this match. This is required for tags and scopes</param>
    /// <param name="scanner">Scanner for this match. This is used for the final output</param>
    /// <param name="offset">Start of the match</param>
    /// <param name="length">Number of characters in the match</param>
    /// <param name="previous">Parser match before this one, to be part of prev/next chain</param>
    internal ParserMatch(IParser source, IScanner scanner, int offset, int length, ParserMatch? previous)
    {
        SourceParser = source;
        Scanner = scanner;

        Offset = offset;
        Length = length;
        Success = Length >= 0;
        Right = length > 0 ? offset + length : offset;
        Previous = previous;
/*
        if (previous is not null && (previous.Next is null || previous.Next.Length < 0))
        {
            previous.Next = this;
        }*/
    }

    /// <summary>
    /// Create a match from a string, that represents that entire string.
    /// This is used to represent transformed values in <see cref="TreeNode{T}"/>s.
    /// </summary>
    internal ParserMatch(string value, string? tag = null)
    {
        SourceParser = new NullParser("Match");
        SourceParser.Tag = tag;

        Scanner = new ScanStrings(value);
        Offset = 0;
        Length = value.Length;
        Success = Length >= 0;
        Right = Offset + Length;
    }

    #region Public

    /// <summary>
    /// List of child matches (for traversing the syntax tree)
    /// </summary>
    public IEnumerable<ParserMatch> Children()
    {
        if (SrcLeftChild is not null) yield return SrcLeftChild;
        if (SrcRightChild is not null) yield return SrcRightChild;
    }

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
            var chainResult = right.Scanner.CreateMatch(source, right.Offset, right.Length, previous);
            if (!right.Empty) chainResult.AddChild(right);
            left?.Scanner.Absorb(left);
            return chainResult;
        }

        if (left.Scanner != right.Scanner) throw new ArgumentException("Can't Join between different scanners");

        // Reduce overlapping matches, if it doesn't lose information
        if ((left.Contains(right) && NoMeta(left, right)) || right.Empty)
        {
            var leftOnlyResult = left.Scanner.CreateMatch(source, left.Offset, left.Length, previous);
            if (!left.Empty) leftOnlyResult.AddChild(left);
            right.Scanner.Absorb(right);
            return leftOnlyResult;
        }

        if ((right.Contains(left) && NoMeta(left, right)) || left.Empty)
        {
            var rightOnlyResult = right.Scanner.CreateMatch(source, right.Offset, right.Length, previous);
            if (!right.Empty) rightOnlyResult.AddChild(right);
            return rightOnlyResult;
        }

        var length     = right.Right - left.Offset;
        var joinResult = left.Scanner.CreateMatch(source, left.Offset, length, previous);


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
        else left.Scanner.Absorb(left);

        if (!right.Empty) joinResult.AddChild(right);
        else right.Scanner.Absorb(right);

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
        var joinResult = left.Scanner.CreateMatch(source, left.Offset, length, previous);
        joinResult.AddChild(left);
        joinResult.AddChild(right);
        return joinResult;
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
        while (true)
        {
            if (node is null) yield break;

            if (select(node)) yield return node; // this match

            if (node.SrcLeftChild is not null)
            {
                foreach (var lm in DepthFirstWalk(node.SrcLeftChild, select))
                {
                    yield return lm;
                }
            }

            if (node.SrcRightChild is not null)
            {
                node = node.SrcRightChild;
                continue;
            }

            break;
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

        // If the existing match doesn't have any meta, just copy ourselves in.
        // Don't do this if our child set is full, as that might break later joins
        if (!HasMetaData() && SrcRightChild is null)
        {
            SourceParser = source;
            return this;
        }

        // Make a match covering this one, with the new source
        var joinMatch = Scanner.CreateMatch(source, Offset, Length, previous);

        // Join this match to the result if we have any metadata to carry
        if (AnyMetaInTree()) joinMatch.AddChild(this);

        return joinMatch;
    }

    /// <summary>
    /// Find and return the first match with the given tag.
    /// Returns <c>null</c> if none found.
    /// </summary>
    public ParserMatch? GetTag(string tag)
    {
        return DepthFirstWalk(this, m => m.Tag == tag).FirstOrDefault();
    }

    /// <summary>
    /// Find and return all matches with the given tag.
    /// Returns empty if none found.
    /// </summary>
    public IEnumerable<ParserMatch> FindByTag(string tag)
    {
        return DepthFirstWalk(this, m => m.Tag == tag);
    }


    /// <summary>
    /// Find and return all matches whose parser matches the given type.
    /// Type can be a direct parser type, or an interface that it implements.
    /// Returns empty if none found.
    /// </summary>
    public IEnumerable<ParserMatch<T>> FindByParserType<T>()
    {
        return DepthFirstWalk(this, m => {
            return m.SourceParser is T;
        }).Select(m => new ParserMatch<T>(m));
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
        return Scanner.CreateMatch(SourceParser, Offset, Length, Previous);
    }

    #endregion Public

    #region Internal

    /// <summary>
    /// Create a copy of this parser match, with a new source parser (used for Wrapper)
    /// </summary>
    internal ParserMatch ReSource(IParser newSource)
    {
        var match = Scanner.CreateMatch(newSource, Offset, Length, Previous);
        match.SrcLeftChild = SrcLeftChild;
        match.SrcRightChild = SrcRightChild;
        return match;
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

    private bool AnyMetaInTree()
    {
        if (SourceParser.HasMetaData()) return true;

        if (SrcLeftChild is not null && SrcLeftChild.AnyMetaInTree()) return true;
        if (SrcRightChild is not null && SrcRightChild.AnyMetaInTree()) return true;
        return false;
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
    /// Add a child match. There are a maximum of two children per match.
    /// Each child may have its own children.
    /// </summary>
    private void AddChild(ParserMatch child)
    {
        if (SrcLeftChild is null) SrcLeftChild = child;
        else if (SrcRightChild is null) SrcRightChild = child;
        else // ReSharper disable once InvocationIsSkipped
            Debug.Assert(false, "Too many children");
    }

    /// <summary>
    /// Remove references to other matches
    /// </summary>
    internal void Reset()
    {
        SrcLeftChild = null;
        SrcRightChild = null;
        Previous = null;
    }

    /// <summary>
    /// Change the content of a parser match
    /// </summary>
    internal void ResetTo(IParser source, IScanner scanner, int offset, int length, ParserMatch? previous)
    {
        SourceParser = source;
        Scanner = scanner;

        Offset = offset;
        Length = length;
        Success = Length >= 0;
        Right = length > 0 ? offset + length : offset;
        Previous = previous;
    }
    #endregion Internal
}