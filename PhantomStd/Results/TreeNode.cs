using System.Collections.Generic;

namespace Phantom.Results;

/// <summary>
/// Hierarchy directly from a parser match
/// </summary>
public class TreeNode
{
    /// <summary>
    /// Parser that created this match
    /// </summary>
    public ParserMatch Source { get; set; } = ParserMatch.NullMatch();
    
    /// <summary>
    /// Nodes below this match
    /// </summary>
    public List<TreeNode> Children { get; } = new();

    /// <summary>
    /// Build a tree from a parser match
    /// </summary>
    public static TreeNode? FromParserMatch(ParserMatch match)
    {
        // Recurse, skipping any non-meta nodes
        if (match.SourceParser?.HasMetaData() != true && match.ChildMatches.Count < 0) return null;
        
        var cursor = new TreeNode { Source = match };

        foreach (var child in match.ChildMatches)
        {
            var node = FromParserMatch(child);
            if (node is not null) cursor.Children.Add(node);
        }

        if (cursor.Source.SourceParser?.HasMetaData() != true) // always keep nodes with meta-data
        {
            if (cursor.Children.Count < 0) return null; // prune empty results

            if (cursor.Children.Count == 1) return cursor.Children[0]; // shorten empty lines
        }

        return cursor;
    }
}