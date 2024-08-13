using System;
using System.Collections.Generic;

namespace Phantom.Results;

/// <summary>
/// Hierarchy derived from a <see cref="ParserMatch"/> tree.
/// This can be manipulated and transformed, using visitor functions.
/// </summary>
public class TreeNode
{
    /// <summary>
    /// Parser that created this match
    /// </summary>
    public ParserMatch Source { get; private set; } = ParserMatch.NullMatch();
    
    /// <summary>
    /// Nodes below this match
    /// </summary>
    public List<TreeNode> Children { get; } = new();

    /// <summary>
    /// Build a tree from a parser match
    /// </summary>
    /// <param name="match">Top parser match to process</param>
    /// <param name="prune">If true, empty nodes will be removed</param>
    public static TreeNode? FromParserMatch(ParserMatch match, bool prune)
    {
        var tree = BasicTreeFromMatch(match);

        if (prune) tree = PruneTree(tree);
        
        return PivotTree(tree);
    }
    
    /// <summary>
    /// Build a tree node from a single string value
    /// </summary>
    public static TreeNode FromString(string match, string? tag = null)
    {
        return new TreeNode
        {
            Source = new ParserMatch(match, tag)
        };
    }

    /// <summary>
    /// Apply a transform operation to all nodes in this tree.
    /// This is repeated until no more transform are made.
    /// <p/>
    /// <b>Important</b>: if your transform always returns new nodes, this method
    /// will continue forever.
    /// </summary>
    /// <param name="node">Root node of tree</param>
    /// <param name="transform">
    /// <p>Function that takes an existing node, and returns its reduced form.</p>
    /// <p>If no reduction is possible, return the given node</p>
    /// <p>If the node should be entirely removed, return <c>null</c></p>
    /// </param>
    public static TreeNode? TransformTree(TreeNode? node, Func<TreeNode, TreeNode?> transform)
    {
        if (node is null) return null;
        
        var cursor = node;
        while (true)
        {
            var changes = false;
            cursor = ApplyTransformRec(cursor, transform, ref changes);
            if (!changes) return cursor;
        }
    }

    #region Inner workings
    
    
    /// <summary>
    /// If a node has no meta-data, and one or zero children;
    /// then remove that node, attaching children to their grandparent.
    /// </summary>
    private static TreeNode? ApplyTransformRec(TreeNode? node, Func<TreeNode, TreeNode?> transform, ref bool changes)
    {
        // Apply to this node. If any changes, return
        if (node is null) return null;
        
        var newNode = transform(node);
        if (newNode is null)
        {
            changes = true;
            return null;
        }

        if (node != newNode)
        {
            changes = true;
            return newNode;
        }

        // No changes to this node, go down to children
        for (var index = 0; index < node.Children.Count; index++)
        {
            var before = node.Children[index];
            var after = ApplyTransformRec(before, transform, ref changes);

            if (after != before) changes = true;
            
            if (after is null)
            {
                node.Children.RemoveAt(index);
                index--;
                continue;
            }
            node.Children[index] = after;
        }

        return node;
    }
    
    
    /// <summary>
    /// If a node has no meta-data, and one or zero children;
    /// then remove that node, attaching children to their grandparent.
    /// </summary>
    private static TreeNode? PruneTree(TreeNode? node)
    {
        if (node is null) return null;
        
        for (var index = 0; index < node.Children.Count; index++)
        {
            var child = PruneTree(node.Children[index]);
            if (child is null)
            {
                node.Children.RemoveAt(index);
                index--;
                continue;
            }
            node.Children[index] = child;
            
            if (!child.Source.HasMetaData()) // if this node is not interesting on its own,
            {
                if (child.Children.Count == 1) // pull single child up
                {
                    node.Children[index] = child.Children[0];
                }
                else if (child.Children.Count < 1) // remove this node
                {
                    node.Children.RemoveAt(index);
                    index--;
                }
            }
        }

        return node;
    }

    /// <summary>
    /// Build a tree from a parser match
    /// </summary>
    private static TreeNode? BasicTreeFromMatch(ParserMatch match)
    {
        // Recurse, skipping any non-meta nodes
        if (!match.HasMetaData() && match.ChildMatches.Count < 0) return null;
        
        var cursor = new TreeNode { Source = match };

        foreach (var child in match.ChildMatches)
        {
            var node = BasicTreeFromMatch(child);
            if (node is not null) cursor.Children.Add(node);
        }

        if (!cursor.Source.HasMetaData()) // always keep nodes with meta-data
        {
            if (cursor.Children.Count < 0) return null; // prune empty results

            if (cursor.Children.Count == 1) return cursor.Children[0]; // shorten empty lines
        }

        return cursor;
    }

    /// <summary>
    /// Push non-pivot peers of pivot nodes down.
    /// <p/>
    /// The new parent is the last pivot node in order,
    /// or the first pivot if non-pivot nodes come first.
    /// </summary>
    private static TreeNode? PivotTree(TreeNode? node)
    {
        if (node is null) return null;

        TreeNode? lastPivot = null;
        var prePivot = new List<TreeNode>();
        for (var index = 0; index < node.Children.Count; index++)
        {
            var child = PivotTree(node.Children[index]);
            if (child is null)
            {
                node.Children.RemoveAt(index);
                index--;
                continue;
            }
            node.Children[index] = child;
            
            if (child.Source.Scope == ScopeType.Pivot)
            {
                lastPivot = child;
                foreach (var p in prePivot)
                {
                    lastPivot.Children.Add(p);
                    node.Children.Remove(p);
                }

                prePivot.Clear();
            }
            else if (lastPivot is not null)
            {
                lastPivot.Children.Add(child);
                node.Children.Remove(child);
                index--;
            }
            else
            {
                prePivot.Add(child);
            }
        }

        prePivot.Clear();

        return node;
    }
    #endregion Inner workings
}