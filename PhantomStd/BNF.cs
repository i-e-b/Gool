using System;
using System.Text.RegularExpressions;
using JetBrains.Annotations;
using Phantom.Parsers;
using Phantom.Parsers.Composite;
using Phantom.Parsers.Terminals;
using Phantom.Results;
using Phantom.Scanners;

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
/// The last item <c>a</c> must be terminated </dd>
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
/// <remarks>
///	See <a href="https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/expressions#1243-operator-overloading">C# language spec</a>
/// </remarks>
// ReSharper disable once InconsistentNaming
public class BNF
{
	// Inspired by Spirit parser http://boost-spirit.com/home/
	// There are a few changes compared to Spirit, all due to overloading
	// restrictions in C#.
	//   1) >> replaced with >			(C# needs one operand of >> to be an integer)
	//   2) * replaced with -			(C# has no normal pointer math, so no unary * )

	/// <summary>
	/// Internal reference to the real parser instance
	/// </summary>
	private readonly IParser _parserTree;

	/// <summary>
	/// Create a BNF wrapper for an <see cref="IParser"/> instance
	/// </summary>
	private BNF(IParser parserTree)
	{
		_parserTree = parserTree;
	}

	/// <summary>
	/// Regular expression options passed to a regexes build with BNF
	/// </summary>
	public static RegexOptions RegexOptions { get; set; }


	/// <summary>
	/// Access the <see cref="IParser"/> resulting from the BNF syntax.
	/// For most cases, you probably want <see cref="ParseString"/>
	/// </summary>
	public IParser Parser()
	{
		return _parserTree;
	}

	/// <summary>
	/// Parse an input string, returning a match tree.
	/// </summary>
	public ParserMatch ParseString(string input, Options options = Options.None)
	{
		var scanner = new ScanStrings(input);
		
		if (options.HasFlag(Options.SkipWhitespace)) scanner.SkipWhitespace = true;
		if (options.HasFlag(Options.IgnoreCase)) scanner.Transform = new TransformToLower();

		var result = _parserTree.Parse(scanner);
		(scanner as IScanningDiagnostics).Complete();
		return result;
	}

	/// <summary>
	/// Wrap this BNF up with the correct scanner options,
	/// ready for user to pass in strings.
	/// </summary>
	public Package WithOptions(Options options)
	{
		return new Package(this, options);
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
	/// Mark this parser as the start of a block
	/// </summary>
	public BNF OpenScope()
	{
		_parserTree.Scope = ScopeType.OpenScope;
		return this;
	}
	
	/// <summary>
	/// Mark this parser as the end of a block
	/// </summary>
	public BNF CloseScope()
	{
		_parserTree.Scope = ScopeType.CloseScope;
		return this;
	}
	
	/// <summary>
	/// Mark this parser as a parent to its siblings.
	/// This alters the way parser results are joined,
	/// pushing pivot matches up, and non-pivot matches down.
	/// </summary>
	public BNF PivotScope()
	{
		_parserTree.Scope = ScopeType.Pivot;
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
		hold.Source = src.Parser();

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

		return new BNF(new Sequence(a.Parser(), b.Parser()));
	}

	/// <summary>
	/// Terminated list parser that matches a list of <i>left side</i>,
	/// each being terminated by one of <i>right side</i>
	/// The last item must be terminated.
	/// </summary>
	public static BNF operator <(BNF a, BNF b)
	{
		if (a == null)
			throw new ArgumentNullException(nameof(a), "Left side of list parser is null");
		if (b == null)
			throw new ArgumentNullException(nameof(b), "Right side of list parser is null");

		return new BNF(new TerminatedList(a.Parser(), b.Parser()));
	}

	/// <summary>
	/// Optional list parser that matches zero or more of the <i>right side</i>
	/// </summary>
	public static BNF operator -(BNF a)
	{
		if (a == null)
			throw new ArgumentNullException(nameof(a), "Loop parser is null");

		return new BNF(new Repetition(a.Parser(), 0, int.MaxValue));
	}

	/// <summary>
	/// List parser that matches one or more of the <i>right side</i>
	/// </summary>
	public static BNF operator +(BNF a)
	{
		if (a == null)
			throw new ArgumentNullException(nameof(a), "Loop parser is null");

		return new BNF(new Repetition(a.Parser(), 1, int.MaxValue));
	}

	/// <summary>
	/// Option parser that matches zero or one of the <i>right side</i>
	/// </summary>
	public static BNF operator !(BNF a)
	{
		if (a == null)
			throw new ArgumentNullException(nameof(a), "Option parser is null");

		return new BNF(new Repetition(a.Parser(), 0, 1));
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

		return new BNF(new DelimitedList(a.Parser(), b.Parser()));
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

		return new BNF(new Union(a.Parser(), b.Parser()));
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

		return new BNF(new Intersection(a.Parser(), b.Parser()));
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

		return new BNF(new Difference(a.Parser(), b.Parser()));
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

		return new BNF(new Exclusive(a.Parser(), b.Parser()));
	}

	/// <summary>
	/// Create a copy of this BNF, with a new tag
	/// </summary>
	public BNF Tagged(string name)
	{
		return Copy().Tag(name);
	}

	/// <summary>
	/// Match a regular expression
	/// </summary>
	public static BNF Regex([RegexPattern]string pattern)
	{
		return new BNF(new RegularExpression(pattern, RegexOptions));
	}
	
	/// <summary>
	/// Match any single character from the given set
	/// </summary>
	public static BNF OneOf(params char[] characters)
	{
		return new BNF(new LiteralCharacterSet(characters));
	}

	/// <summary>
	/// Match any single character as long as its <b>NOT</b> in the given set
	/// </summary>
	public static BNF NoneOf(params char[] characters)
	{
		return new BNF(new ExcludingCharacterSet(characters));
	}

	/// <summary>
	/// Match any one character
	/// </summary>
	public static BNF AnyChar => new(new AnyCharacter());
	
	/// <summary>
	/// Match an empty string
	/// </summary>
	public static BNF Empty => new(new EmptyMatch());
	
	/// <summary>
	/// Match the end of a line
	/// </summary>
	public static BNF LineEnd => new(new EndOfLine());

	/// <summary>
	/// Set the given tag on all items
	/// </summary>
	public static void TagAll(string tag, params BNF[] items)
	{
		foreach (var item in items)
		{
			item.Tag(tag);
		}
	}
	
	/// <summary>
	/// Match the end of input
	/// </summary>
	public static BNF EndOfInput => new(new EndOfInput());
	
	/// <summary>
	/// Match a single character of white-space
	/// </summary>
	public static BNF WhiteSpace => new(new Whitespace());

	/// <summary>
	/// Create a forward reference to populate later.
	/// This enables recursive definitions.
	/// </summary>
	public static BnfForward Forward()
	{
		return new BnfForward(new Recursion());
	}


	/// <summary>
	/// Options for parsing
	/// </summary>
	[Flags]
	public enum Options
	{
		/// <summary>
		/// No special options. Parser must match input exactly.
		/// </summary>
		None = 0,
		
		/// <summary>
		/// Skip white-space characters between matches.
		/// White-space inside matches is preserved.
		/// </summary>
		SkipWhitespace = 1,
		
		/// <summary>
		/// Lower-case the input before applying parsers.
		/// Case is preserved in output.
		/// Parsers should expect lowercase input
		/// </summary>
		IgnoreCase = 2,
	}
	
	/// <summary>
	/// BNF structure, plus the correct scanner options
	/// </summary>
	public class Package
	{
		private readonly BNF _bnf;
		private readonly Options _options;

		/// <summary>
		/// BNF structure, plus the correct scanner options
		/// </summary>
		internal Package(BNF bnf, Options options)
		{
			_bnf = bnf;
			_options = options;
		}
		
		/// <summary>
		/// Parse an input string, returning a match tree.
		/// </summary>
		public ParserMatch ParseString(string input)
		{
			return _bnf.ParseString(input, _options);
		}
	}

	/// <summary>
	/// BNF forward reference, to be completed at the end of BNF definition.
	/// This allows recursive definitions.
	/// </summary>
	public class BnfForward : BNF
	{
		/// <summary>
		/// Create a forward reference
		/// </summary>
		public BnfForward(IParser parserTree) : base(parserTree)
		{
		}

		/// <summary>
		/// Complete a forward reference with the completed parser
		/// </summary>
		public void Is(IParser parser)
		{
			if (_parserTree is not Recursion rec) throw new Exception($"Invalid forward reference. Expected '{nameof(Recursion)}', got '{_parserTree.GetType().Name}'");
			rec.Source = parser;
		}
	
		/// <summary>
		/// Complete a forward reference with the completed parser
		/// </summary>
		public void Is(BNF parser)
		{
			if (_parserTree is not Recursion rec) throw new Exception($"Invalid forward reference. Expected '{nameof(Recursion)}', got '{_parserTree.GetType().Name}'");
			rec.Source = parser.Parser();
		}
	}
}