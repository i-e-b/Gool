using System.Collections.Generic;

namespace Gool.Parsers.Composite.Abstracts;

/// <summary>
/// A composition that takes only one parser.
/// (this is a non-composite adaptor)
/// </summary>
public abstract class Unary : Parser
{
	/// <summary>
	/// Create a composite parser from a single input parser
	/// </summary>
	protected Unary(IParser parser)
	{
		Parser = parser;
	}

	/// <inheritdoc />
	public override IEnumerable<IParser> ChildParsers() {
		yield return Parser;
	}

	/// <summary>
	/// The base parser
	/// </summary>
	protected IParser Parser { get; set; }
}