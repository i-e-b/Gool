using System;
using System.Text.RegularExpressions;
using Phantom.Parsers;
using Phantom.Parsers.Composite;
using Phantom.Parsers.Terminals;

namespace Phantom;

/// <summary>
/// Provides a <i>Backus–Naur form</i>-like syntax for building parsers.<br/>
/// <p/>
/// <b>Atomic parsers:</b>
/// <dl>
/// 
/// <dt><![CDATA[ "#..." ]]></dt>
/// <dd>Create a <b>regex</b> parser that matches a string based on a regex pattern. The <c>#</c> prefix is not included in the pattern</dd>
/// 
/// <dt><![CDATA[ '.' ]]></dt>
/// <dd>Create a <b>character</b> parser that matches a single literal character in the input</dd>
/// 
/// <dt><![CDATA[ "..." ]]></dt>
/// <dd>Create a <b>string</b> parser that matches a literal string in the input</dd>
///
/// </dl>
///
/// <b>Combining parsers:</b>
/// <dl>
/// 
/// <dt><![CDATA[ a < b ]]></dt>
/// <dd>Create a <b>terminated list</b> parser that matches a list of <c>a</c>, each being terminated by <c>b</c>
/// The last item <c>a</c> may be terminated, but need not be.</dd>
/// 
/// <dt><![CDATA[ a > b ]]></dt>
/// <dd>Create a <b>sequence</b> parser that matches <c>a</c> then <c>b</c></dd>
/// 
/// <dt><![CDATA[ -a ]]></dt>
/// <dd>Create an <b>optional repeat</b> parser that matches zero or more <c>a</c></dd>
/// 
/// <dt><![CDATA[ +a ]]></dt>
/// <dd>Create an <b>repeat</b> parser that matches one or more <c>a</c></dd>
/// 
/// <dt><![CDATA[ !a ]]></dt>
/// <dd>Create an <b>option</b> parser that matches zero or one <c>a</c></dd>
/// 
/// <dt><![CDATA[ a % b ]]></dt>
/// <dd>Create a <b>delimited list</b> parser that matches a list of <c>a</c>, delimited by <c>b</c></dd>
/// 
/// <dt><![CDATA[ a | b ]]></dt>
/// <dd>Create a <b>union</b> parser that matches the longest result from either <c>a</c> or <c>b</c>. Parser will match if both <c>a</c> and <c>b</c> match</dd>
/// 
/// <dt><![CDATA[ a & b ]]></dt>
/// <dd>Create an <b>intersection</b> parser that matches (<c>a</c> then <c>b</c>) or (<c>b</c> then <c>a</c>)</dd>
/// 
/// <dt><![CDATA[ a ^ b ]]></dt>
/// <dd>Create an <b>exclusion</b> parser that matches <c>a</c> or <c>b</c> but not both</dd>
/// 
/// <dt><![CDATA[ a / b ]]></dt>
/// <dd>Create an <b>difference</b> parser that matches <c>a</c> but not <c>b</c></dd>
/// 
/// </dl>
/// </summary>
// ReSharper disable once InconsistentNaming
public class BNF
{
	// Inspired by Spirit parser http://boost-spirit.com/home/
	// There are a few changes compared to Spirit, all due to overloading
	// restrictions in C#.
	//   1) >> replaced with >			(C# needs one operand of >> to be an integer)
	//   2) * replaced with -			(C# has no normal pointer math, so no unary * )

	private readonly IParser _parserTree;

	/// <summary>
	/// Create a BNF wrapper for an <see cref="IParser"/> instance
	/// </summary>
	public BNF(IParser parserTree)
	{
		_parserTree = parserTree;
	}

	/// <summary>
	/// Regular expression options passed to a regexes build with BNF
	/// </summary>
	public static RegexOptions RegexOptions { get; set; }

	/// <summary>
	/// Match the end of input
	/// </summary>
	public static BNF EndOfInput => new(new EndOfInput());

	/// <summary>
	/// Parser resulting from the BNF syntax
	/// </summary>
	/// <returns></returns>
	public IParser Result()
	{
		return _parserTree;
	}

	/// <summary>
	/// Add a tag to the base parser.
	/// This is used to interpret the parser result
	/// </summary>
	public BNF Tag(string tag)
	{
		_parserTree.Tag = tag;
		return this;
	}
	
	/// <summary>
	/// Add a tag to the base parser.
	/// This is used to interpret the parser result
	/// </summary>
	public BNF OpenScope()
	{
		_parserTree.ScopeSign = +1;
		return this;
	}
	
	/// <summary>
	/// Add a tag to the base parser.
	/// This is used to interpret the parser result
	/// </summary>
	public BNF CloseScope()
	{
		_parserTree.ScopeSign = -1;
		return this;
	}

	/// <summary>
	/// Make a copy of this BNF, which can be given different tags from the original
	/// </summary>
	public BNF Copy()
	{
		return new BNF(new Wrapper(_parserTree));
	}

	/// <summary>
	/// Create a self-recursive parser structure.
	/// <p/>
	/// The function is given a 
	/// <p/>
	/// See <see cref="Recursion"/>
	/// </summary>
	/// <example><code><![CDATA[
	/// var input = new ScanStrings("{{{w}{x}y}z}");
	/// var myParser = BNF.Recursive(tree => +( '{' > -(tree | "#[^{}]+") > '}')).Result();
	/// var result = myParser.Parse(input);
	/// ]]></code>
	/// </example>
	public static BNF Recursive(Func<BNF, BNF> parserTreeFunction)
	{
		// BNF.Recursive(tree => !(open_tag > -(text | tree) > close_tag)).Result();
		// The way this works involves a lot of bad-practice and hidden typecasts.
		var hold = new Recursion();

		var src = parserTreeFunction(hold);
		hold.Source = src.Result();

		return hold;
	}

	/// <summary>
	/// Convert a character into a parser
	/// </summary>
	/// <remarks>
	/// This is a lovely feature of C#. It makes a lot of redundant code disappear,
	/// and lets us get away with linguistic murder!
	/// Whoever made this part of the language, I salute you!
	/// </remarks>
	public static implicit operator BNF(char c)
	{
		return new BNF(new LiteralCharacter(c));
	}

	/// <summary>
	/// String parser
	/// <ul>
	/// <li>If the string starts with '#' it will be treated as a regular expression, with the initial '#' removed</li>
	/// <li>If the string starts with '##', this parser will match a literal string starting with a single '#'</li>
	/// <li>Otherwise, the parser matches the literal string given</li>
	/// </ul>
	/// </summary>
	public static implicit operator BNF(string s)
	{
		string pattern;
		if (s.StartsWith("#"))
		{
			pattern = s.Substring(1);
			if (!pattern.StartsWith("#"))
			{
				return new BNF(new RegularExpression(pattern, RegexOptions));
			}
		}
		else
		{
			pattern = s;
		}
		return new BNF(new LiteralString(pattern));
	}
		
	/// <summary>
	/// Implicitly wrap a parser in a BNF syntax helper
	/// </summary>
	public static implicit operator BNF(Parser p)
	{
		return new BNF(p);
	}

	/// <summary>
	/// Sequence parser that matches <i>left side</i> then <i>right side</i>
	/// </summary>
	public static BNF operator >(BNF a, BNF b)
	{
		if (a == null)
			throw new ArgumentNullException(nameof(a), "Left side of sequence parser is null");
		if (b == null)
			throw new ArgumentNullException(nameof(b), "Right side of sequence parser is null");

		return new BNF(new Sequence(a.Result(), b.Result()));
	}

	/// <summary>
	/// Terminated list parser that matches a list of <i>left side</i>,
	/// each being terminated by one of <i>right side</i>
	/// The last item may be terminated, but this is not required.
	/// </summary>
	public static BNF operator <(BNF a, BNF b)
	{
		if (a == null)
			throw new ArgumentNullException(nameof(a), "Left side of list parser is null");
		if (b == null)
			throw new ArgumentNullException(nameof(b), "Right side of list parser is null");

		return new BNF(new TerminatedList(a.Result(), b.Result()));
	}

	/// <summary>
	/// Optional list parser that matches zero or more of the <i>right side</i>
	/// </summary>
	public static BNF operator -(BNF a)
	{
		if (a == null)
			throw new ArgumentNullException(nameof(a), "Loop parser is null");

		return new BNF(new Repetition(a.Result(), 0, int.MaxValue));
	}

	/// <summary>
	/// List parser that matches one or more of the <i>right side</i>
	/// </summary>
	public static BNF operator +(BNF a)
	{
		if (a == null)
			throw new ArgumentNullException(nameof(a), "Loop parser is null");

		return new BNF(new Repetition(a.Result(), 1, int.MaxValue));
	}

	/// <summary>
	/// Option parser that matches zero or one of the <i>right side</i>
	/// </summary>
	public static BNF operator !(BNF a)
	{
		if (a == null)
			throw new ArgumentNullException(nameof(a), "Option parser is null");

		return new BNF(new Repetition(a.Result(), 0, 1));
	}

	/// <summary>
	/// Delimited list parser that matches a list of <i>left side</i>, separated by <i>right side</i>
	/// </summary>
	public static BNF operator %(BNF a, BNF b)
	{
		if (a == null)
			throw new ArgumentNullException(nameof(a), "Left side of list parser is null");
		if (b == null)
			throw new ArgumentNullException(nameof(b), "Right side of list parser is null");

		return new BNF(new DelimitedList(a.Result(), b.Result()));
	}

	/// <summary>
	/// Union parser that matches the longest match of <i>left side</i> or <i>right side</i>.
	/// Both sides of the union can match. See also <c>^</c> (<see cref="Exclusive"/>)
	/// </summary>
	public static BNF operator |(BNF a, BNF b)
	{
		if (a == null)
			throw new ArgumentNullException(nameof(a), "Left side of union parser is null");
		if (b == null)
			throw new ArgumentNullException(nameof(b), "Right side of union parser is null");

		return new BNF(new Union(a.Result(), b.Result()));
	}

	/// <summary>
	/// Intersection parser that matches either (<i>left</i> then <i>right</i>) or (<i>right</i> then <i>left</i>)
	/// </summary>
	public static BNF operator &(BNF a, BNF b)
	{
		if (a == null)
			throw new ArgumentNullException(nameof(a), "Left side of intersection parser is null");
		if (b == null)
			throw new ArgumentNullException(nameof(b), "Right side of intersection parser is null");

		return new BNF(new Intersection(a.Result(), b.Result()));
	}
	
	/// <summary>
	/// Difference parser that matches <i>left</i> but not <i>right</i>
	/// </summary>
	public static BNF operator /(BNF a, BNF b)
	{
		if (a == null)
			throw new ArgumentNullException(nameof(a), "Left side of difference parser is null");
		if (b == null)
			throw new ArgumentNullException(nameof(b), "Right side of difference parser is null");

		return new BNF(new Difference(a.Result(), b.Result()));
	}

	/// <summary>
	/// Exclusive-Or parser that matches <i>left side</i> or <i>right side</i> but <b>not</b> both
	/// If both sides match, this parser will not match. See also <c>|</c> (<see cref="Union"/>)
	/// </summary>
	public static BNF operator ^(BNF a, BNF b)
	{
		if (a == null)
			throw new ArgumentNullException(nameof(a), "Left side of Exclusive-Or parser is null");
		if (b == null)
			throw new ArgumentNullException(nameof(b), "Right side of Exclusive-Or parser is null");

		return new BNF(new Exclusive(a.Result(), b.Result()));
	}

	/// <summary>
	/// Create a copy of this BNF, with a new tag
	/// </summary>
	public BNF Tagged(string name)
	{
		return Copy().Tag(name);
	}

	/// <summary>
	/// Match any single character from the given set
	/// </summary>
	public static BNF OneOf(params char[] characters)
	{
		return new BNF(new LiteralCharacterSet(characters));
	}
}