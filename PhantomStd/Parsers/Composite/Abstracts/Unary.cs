using System.Collections.Generic;
using Phantom.Parsers.Interfaces;
using Phantom.Results;

namespace Phantom.Parsers.Composite.Abstracts;

/// <summary>
/// A composition that takes only one parser.
/// (this is a non-composite adaptor)
/// </summary>
public abstract class Unary : Parser, ICompositeParser
{
	/// <summary>
	/// Create a composite parser from a single input parser
	/// </summary>
	protected Unary(IParser parser)
	{
		Parser = parser;
	}

	/// <summary>
	/// The base parser
	/// </summary>
	public IParser Parser { get; set; }

	/// <inheritdoc />
	public List<IParser> ChildParsers()
	{
		return new List<IParser> { Parser };
	}

	/// <inheritdoc />
	public abstract ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch);
}