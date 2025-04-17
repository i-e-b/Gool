namespace Gool.Results;

/// <summary>
/// Change in tree scope that this match represents
/// </summary>
public enum ScopeType
{
    /// <summary> This match is general data </summary>
    None = 0,
	
    /// <summary>
    /// This match should become a parent to its siblings.
    /// This alters the way parser results are joined,
    /// pushing pivot matches up, and non-pivot matches down.
    /// </summary>
    Pivot = 2,
    
    /// <summary>
    /// This match represents the contents of a scope -- it is both an opening and closing match
    /// </summary>
    Enclosed = 3,

    /// <summary>
    /// This match should be kept separate in the results tree, even if it does not have a tag
    /// </summary>
    Tree = 4,
	
    /// <summary> This match is the start of a block </summary>
    OpenScope = 1,
	
    /// <summary> This match is the end of a block </summary>
    CloseScope = -1,
}