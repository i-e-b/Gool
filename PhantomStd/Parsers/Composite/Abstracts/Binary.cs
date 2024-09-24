namespace Gool.Parsers.Composite.Abstracts;

/// <summary>
/// An abstract template for binary composite parsers
/// </summary>
public abstract class Binary : Parser
{
	/// <summary>
	/// Create a binary parser from a pair of left and right side parsers
	/// </summary>
	protected Binary(IParser left, IParser right)
	{
		LeftParser = left;
		RightParser = right;
	}

	/// <summary>
	/// Gets the left-side parser of the binary pair
	/// </summary>
	protected IParser LeftParser { get; set; }

	/// <summary>
	/// Gets the right-side parser of the binary pair
	/// </summary>
	protected IParser RightParser { get; set; }
}