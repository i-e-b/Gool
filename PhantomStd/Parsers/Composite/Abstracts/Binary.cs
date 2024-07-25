using System.Collections.Generic;
using Phantom.Parsers.Interfaces;

namespace Phantom.Parsers.Composite.Abstracts;

/// <summary>
/// An abstract template for binary composite parsers
/// </summary>
public abstract class Binary : Parser, ICompositeParser
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
	public IParser LeftParser { get; set; }

	/// <summary>
	/// Gets the right-side parser of the binary pair
	/// </summary>
	public IParser RightParser { get; set; }

	/// <inheritdoc />
	public List<IParser> ChildParsers()
	{
		var c = new List<IParser>();
		c.Add(LeftParser);
		c.Add(RightParser);
		return c;
	}

	/// <inheritdoc />
	public abstract ParserMatch TryMatch(IScanner scan);
}