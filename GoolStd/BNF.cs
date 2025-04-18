﻿using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Text.RegularExpressions;
using Gool.Parsers;
using Gool.Parsers.Composite;
using Gool.Parsers.Terminals;
using Gool.Parsers.Transforms;
using Gool.Results;
using Gool.Scanners;
using JetBrains.Annotations;

namespace Gool;

/// <summary>
/// Provides a <i>Backus–Naur form</i>-like syntax for building parsers.<br/>
/// <p/>
/// <b>Atomic parsers:</b>
/// <dl>
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
/// <dt><![CDATA[ ~a ]]></dt>
/// <dd>Create an <b>non-consuming</b> parser that must match <c>a</c>, but does not consume that match</dd>
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
public class BNF : IParser
{
	// Inspired by Spirit parser http://boost-spirit.com/home/
	// There are a few changes compared to Spirit, all due to overloading restrictions in C#.
	//   1) >> replaced with >			(C# needs one operand of >> to be an integer)
	//   2) * replaced with -			(C# has no normal pointer math, so no unary * )

	/// <summary>
	/// Regular expression options passed to a regexes build with BNF
	/// </summary>
	public static RegexOptions RegexSettings { get; set; }

	/// <summary>
	/// Parse an input string, returning a match tree.
	/// </summary>
	/// <param name="input">String to parse</param>
	/// <param name="offset">Optional: Offset into the input to start parsing</param>
	/// <param name="options">Optional: Settings for parsing, which can significantly change result</param>
	/// <param name="autoAdvance">Optional: Custom auto-advance. This overrides <c>Options.SkipWhitespace</c></param>
	/// <param name="recordDiagnostics">Optional, default <c>true</c>: If true, record diagnostics for error reporting. Parsing is faster without</param>
	/// <param name="mustConsumeAll">
	/// Optional, default = <c>false</c>.
	/// If true, parsing will fail if it does not consume all of the input.</param>
	public ParserMatch ParseString(string input, int offset = 0, Options options = Options.None, IParser? autoAdvance = null, bool recordDiagnostics = true, bool mustConsumeAll = false)
	{
		var scanner = new ScanStrings(input, recordDiagnostics);
		
		if (options.HasFlag(Options.SkipWhitespace)) scanner.AutoAdvance = WhiteSpaceString;
		if (options.HasFlag(Options.IgnoreCase)) scanner.Transform = new TransformToLower();
		if (options.HasFlag(Options.IncludeSkippedElements)) scanner.IncludeSkippedElements = true;

		if (autoAdvance is not null) scanner.AutoAdvance = autoAdvance;

		// Parse as much input as we can
		var result = _parserTree.Parse(scanner, scanner.CreateMatch(this, offset, -1, null));
		(scanner as IScanningDiagnostics).Complete();

		var endPosition = result.Right;

		// If there is trailing insignificant data, consume it
		if (scanner.AutoAdvance is not null)
		{
			var trailing = scanner.AutoAdvance.Parse(scanner, result, false);
			if (trailing.Success)
			{
				if (scanner.IncludeSkippedElements) result = ParserMatch.Join(result, new NullParser("Skipped elements"), result, trailing);
				else endPosition = trailing.Right;
			}
		}

		if (mustConsumeAll && endPosition < input.Length) return scanner.NoMatch(_parserTree, null);
		
		return result;
	}

	/// <summary>
	/// Wrap this BNF up with the correct scanner options,
	/// ready for user to pass in strings.
	/// </summary>
	public Package WithOptions(Options options)
	{
		return new Package(this, options, null);
	}

	/// <summary>
	/// Wrap this BNF up with the correct scanner options,
	/// ready for user to pass in strings.
	/// </summary>
	public Package WithOptions(Options options, IParser autoAdvance)
	{
		return new Package(this, options, autoAdvance);
	}

	#region Tagging
	/// <summary>
	/// Add a tag to the base parser.
	/// This is used to interpret the parser result
	/// </summary>
	public BNF TagWith(string tag)
	{
		_parserTree.Tag = tag;
		return this;
	}

	/// <summary>
	/// Create a copy of this BNF, with a new tag
	/// </summary>
	public BNF Tagged(string name)
	{
		return Copy().TagWith(name);
	}

	/// <summary>
	/// Set the given tag on all items
	/// </summary>
	public static void TagAll(string tag, params BNF[] items)
	{
		foreach (var item in items)
		{
			item.TagWith(tag);
		}
	}

	/// <summary>
	/// Make a copy of this BNF, which can be given different tags from the original
	/// </summary>
	public BNF Copy()
	{
		return new BNF(new Wrapper(_parserTree));
	}
	#endregion Tagging

	#region Scopes

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
	/// Mark this parser as the contents of a scope.
	/// The parser will be both the start and end of the scope
	/// </summary>
	public BNF EncloseScope()
	{
		_parserTree.Scope = ScopeType.Enclosed;
		return this;
	}

	/// <summary>
	/// Mark this parser as part of a scope tree.
	/// Matches will be kept separate in the results tree, even if they do not have a tag
	/// </summary>
	public BNF TreeScope()
	{
		_parserTree.Scope = ScopeType.Tree;
		return this;
	}



	#endregion Scopes

	#region Contextual and recursive
	/// <summary>
	/// Create a self-recursive parser structure.
	/// <p>
	/// The <paramref name="parserTreeFunction"/> is passed a representation of itself,
	/// and should return a new BNF structure which can include this self-referencing parameter.
	/// </p><p>
	/// A recursive parser must make progress through the scanner input to be considered a successful match.
	/// Paths through the parser which recurse without advancing are curtailed.
	/// </p>
	/// </summary>
	/// <example><code><![CDATA[
	/// var myParser = BNF.Recursive(tree => +( '{' > -(tree | BNF.Letter ) > '}')).Result();
	///
	/// var input = new ScanStrings("{{{w}{x}y}z}");
	/// var result = myParser.Parse(input);
	/// ]]></code>
	/// </example>
	/// <seealso cref="Recursion"/>
	public static BNF Recursive(Func<BNF, BNF> parserTreeFunction)
	{
		var hold = new Recursion();

		var src = parserTreeFunction(hold);
		hold.Source = src;

		return hold;
	}

	/// <summary>
	/// Create a forward reference to populate later.
	/// This enables recursive definitions.
	/// </summary>
	public static BnfForward Forward()
	{
		return new BnfForward(new Recursion());
	}

	/// <summary>
	/// Create a contextualised parser from a previous result.
	/// </summary>
	/// <param name="prefix">
	/// Parser that reads context. This must match for the generated parser to be run.
	/// The result tree from this parser will be available to build the 'next' one.
	/// </param>
	/// <param name="select">
	/// Optional function to select parts of the match to use.
	/// If not provided, the entire match will be given.
	/// If the function is given, but returns null, the context will fail to match</param>
	/// <param name="next">
	/// Function to generate the next parser fragment
	/// </param>
	public static BNF Context(BNF prefix, Func<ParserMatch, ParserMatch?>? select, Func<ParserMatch, BNF> next)
	{
		return new ContextParser(prefix, select, next);
	}

	/// <summary>
	/// Test if the previous match's last character matches the given pattern.
	/// Non-capturing: this returns an empty result if the pattern matches,
	/// and a failure if not.
	/// </summary>
	public static BNF PreviousEndsWith(IParser pattern)
	{
		return new PreviousCharacterCheck(pattern);
	}

	/// <summary>
	/// Test if the previous match's last character matches the given pattern.
	/// Non-capturing: this returns an empty result if the pattern matches,
	/// and a failure if not.
	/// </summary>
	public static BNF PreviousEndsWith(BNF pattern)
	{
		return new PreviousCharacterCheck(pattern);
	}

	/// <summary>
	/// Test if the previous non-empty match matches the given pattern.
	/// Non-capturing: this returns an empty result if the pattern matches,
	/// and a failure if not.
	/// </summary>
	/// <param name="select">
	/// Optional function to select parts of the match to use.
	/// If not provided, the entire match will be given.
	/// If the function is given, but returns null, the pattern will fail to match</param>
	/// <param name="pattern">Pattern to try against previous match</param>
	/// <param name="matchEntire">If <c>true</c>, the pattern must match the entire previous match.
	/// Otherwise the match can be partial.</param>
	public static BNF PreviousMatches(Func<ParserMatch, ParserMatch?>? select, BNF pattern, bool matchEntire = true)
	{
		return new PreviousMatchCheck(select, pattern, matchEntire);
	}

	/// <summary>
	/// If this parser matches, test the <b>match text</b> against a further
	/// set of patterns. The final result is only successful if <b>all</b> the given
	/// patterns match the original result.
	/// </summary>
	public BNF WithValidators(params BNF[] validators)
	{
		return new BNF(new ParallelSet(this, validators));
	}

	/// <summary>
	/// Perform an action when the parser is matched.
	/// This modifies the current parser.
	/// </summary>
	public void MatchAction(Action<ParserMatch> action)
	{
		_parserTree = _parserTree.WithMatchAction(action);
	}
	#endregion Contextual and recursive

	#region Operators and implicit conversions

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
		return new BNF(new LiteralCharacterSet(c));
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
		return s.Length < 1
			? new BNF(new EmptyMatch())
			: new BNF(new LiteralString(s));
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

		return new BNF(new Sequence(a, b));
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

		return new BNF(new TerminatedList(a, b));
	}

	/// <summary>
	/// Optional list parser that matches zero or more of the <i>right side</i>
	/// </summary>
	public static BNF operator -(BNF a)
	{
		if (a == null)
			throw new ArgumentNullException(nameof(a), "Loop parser is null");

		return new BNF(new Repetition(a, 0, int.MaxValue));
	}

	/// <summary>
	/// List parser that matches one or more of the <i>right side</i>
	/// </summary>
	public static BNF operator +(BNF a)
	{
		if (a == null)
			throw new ArgumentNullException(nameof(a), "Loop parser is null");

		return new BNF(new Repetition(a, 1, int.MaxValue));
	}

	/// <summary>
	/// Option parser that matches zero or one of the <i>right side</i>
	/// </summary>
	public static BNF operator !(BNF a)
	{
		if (a == null)
			throw new ArgumentNullException(nameof(a), "Option parser is null");

		return new BNF(new Repetition(a, 0, 1));
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

		return new BNF(new DelimitedList(a, b));
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

		return new BNF(new Union(a, b));
	}

	/// <summary>
	/// Creates a Preference-Union (or 'preference-alternative') parser from two sub-parsers.
	/// This returns the first successful match from the left
	/// </summary>
	public static BNF operator >=(BNF a, BNF b)
	{
		if (a == null)
			throw new ArgumentNullException(nameof(a), "Left side of preference-union parser is null");
		if (b == null)
			throw new ArgumentNullException(nameof(b), "Right side of preference-union parser is null");

		return new BNF(new PreferenceUnion(a, b));
	}

	/// <summary>
	/// Not yet implemented
	/// </summary>
	public static BNF operator <=(BNF a, BNF b)
	{
		throw new NotImplementedException();
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

		return new BNF(new Intersection(a, b));
	}
	
	/// <summary>
	/// Negative look-ahead. Difference parser that matches <i>left</i> but not <i>right</i>
	/// </summary>
	public static BNF operator /(BNF a, BNF b)
	{
		if (a == null)
			throw new ArgumentNullException(nameof(a), "Left side of difference parser is null");
		if (b == null)
			throw new ArgumentNullException(nameof(b), "Right side of difference parser is null");

		return new BNF(new Difference(a, b));
	}

	/// <summary>
	/// Positive look-ahead. Non-consuming parser that must match the <i>right side</i>, but does not consume the match
	/// </summary>
	public static BNF operator ~(BNF a)
	{
		if (a == null) throw new ArgumentNullException(nameof(a), "Non-consuming parser is null");

		return new BNF(new NonConsumingMatch(a));
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

		return new BNF(new Exclusive(a, b));
	}

	#endregion Operators and implicit conversions

	#region Composite/combination helpers

	/// <summary>
	/// Turn off auto-advance when matching this pattern and its sub-patterns.
	/// Has no effect if neither <see cref="Options.SkipWhitespace"/> nor
	/// <see cref="Package.AutoAdvance"/> is set.
	/// </summary>
	public void NoAutoAdvance()
	{
		_autoAdvanceEnabled = false;
	}

	/// <summary>
	/// Compact all sub-matches into a single result, regardless of tagging or scopes.
	/// </summary>
	public BNF Atomic()
	{
		_atomic = true;
		return this;
	}

	/// <summary>
	/// Match an ordered sequence of sub-parsers as a single match.
	/// The entire set of sub-parsers must match for a successful match.
	/// </summary>
	public static BNF Composite(IEnumerable<BNF> items)
	{
		return new CompositeSequence(items);
	}

	/// <summary>
	/// Match an ordered sequence of sub-parsers as a single match.
	/// The entire set of sub-parsers must match for a successful match.
	/// </summary>
	public static BNF Composite(params BNF[] items)
	{
		return new CompositeSequence(items);
	}

	/// <summary>
	/// Optional variant of the given pattern.
	/// This is an alias for <c>!opt</c>,
	/// which can be used where it is clearer.
	/// </summary>
	public static BNF Optional(BNF opt)
	{
		return new BNF(new Repetition(opt, 0, 1));
	}

	/// <summary>
	/// Repeat the pattern a specific number of times
	/// </summary>
	public BNF Repeat(int i)
	{
		if (i < 0) throw new Exception("Repeat count must be greater than or equal to zero");
		if (i == 1) return this;
		if (i == 0) return Empty;

		return new BNF(new Repetition(this, (uint)i, (uint)i));
	}

	/// <summary>
	/// Repeat the pattern a range of times
	/// </summary>
	public BNF Repeat(int min, int max)
	{
		return new BNF(new Repetition(this, (uint)min, (uint)max));
	}

	/// <summary>
	/// Repeat the pattern a range of times
	/// </summary>
	public static BNF Repeat(BNF pattern, int min, int max)
	{
		return new BNF(new Repetition(pattern, (uint)min, (uint)max));
	}

	/// <summary>
	/// Match a regular expression
	/// </summary>
	public static BNF Regex([RegexPattern]string pattern)
	{
		return new BNF(new RegularExpression(pattern, RegexSettings));
	}
	
	/// <summary>
	/// Matches the remaining input, only if its length is between
	/// <paramref name="min"/> and <paramref name="max"/> (inclusive)
	/// </summary>
	public static BNF RemainingLength(int min, int max)
	{
		return new BNF(new RemainingLength(min, max));
	}
	
	/// <summary>
	/// Match any single character from the given set
	/// </summary>
	public static BNF OneOf(params char[] characters)
	{
		return new BNF(new LiteralCharacterSet(characters));
	}

	/// <summary>
	/// Match the longest literal string from the given set
	/// </summary>
	public static BNF OneOf(params string[] literals)
	{
		return new BNF(new Union(literals.Select(l=> new LiteralString(l))));
	}

	/// <summary>
	/// Match the longest literal string from the given set
	/// </summary>
	public static BNF OneOf(IEnumerable<string> literals)
	{
		return new BNF(new Union(literals.Select(l=> new LiteralString(l))));
	}

	/// <summary>
	/// Match any single character as long as its <b>NOT</b> in the given set
	/// </summary>
	public static BNF NoneOf(params char[] characters)
	{
		return new BNF(new ExcludingCharacterSet(characters));
	}
	
	/// <summary>
	/// Match a single character that is between <paramref name="lower"/>
	/// and <paramref name="upper"/> (inclusive), which is not in the list of exclusions
	/// </summary>
	public static BNF RangeExcluding(char lower, char upper, params char[] exclusions)
	{
		return new BNF(new RangeExcludingCharacterSet(lower, upper, exclusions));
	}

	/// <summary>
	/// Match a single character inside any of the inclusive ranges
	/// </summary>
	public static BNF CharacterInRanges(params CharacterRange[] ranges)
	{
		return new BNF(new MultiRangeCharacterSet(ranges));
	}

	/// <summary>
	/// Match a single character OUTSIDE ALL of the inclusive ranges
	/// </summary>
	public static BNF CharacterNotInRanges(params CharacterRange[] ranges)
	{
		return new BNF(new MultiRangeExcludingCharacterSet(ranges));
	}
	
	/// <summary>
	/// Create a parser for a fixed width unsigned integer, within a given value range.
	/// </summary>
	/// <param name="min">Inclusive minimum value for result. Must be zero or greater</param>
	/// <param name="max">Inclusive maximum value for result. Must be greater than lower</param>
	/// <param name="width">Number of characters to read from input</param>
	/// <param name="allowLeadingWhitespace">
	/// Default = <c>false</c>. 
	/// If <c>true</c> the input may have leading whitespace to fill the fixed width.
	/// If <c>false</c> the input must have digits in all places.
	/// </param>
	/// <param name="useHex">
	/// Default = <c>false</c>. 
	/// If <c>true</c> the input may have 0-9 and A-F/a-f; Number will be checked against range as a hexadecimal value.
	/// If <c>false</c> the input may have 0-9 only; Number will be checked against range as a decimal value.
	/// </param>
	/// <seealso cref="IntegerRange"/>
	/// <seealso cref="FractionalDecimal"/>
	public static BNF FixedSizeInteger(long min, long max, int width, bool allowLeadingWhitespace = false, bool useHex = false)
	{
		return new BNF(new FixedWidthIntegerRange(min, max, width, allowLeadingWhitespace, useHex));
	}
	
	/// <summary>
	/// Create a parser for a variable width unsigned integer, within a given value range.
	/// </summary>
	/// <param name="min">Inclusive minimum value for result. Must be zero or greater</param>
	/// <param name="max">Inclusive maximum value for result. Must be greater than lower</param>
	/// <param name="allowLeadingWhitespace">
	/// Default = <c>false</c>. 
	/// If <c>true</c> the input may have leading whitespace.
	/// If <c>false</c> the input must have digits in all places.
	/// </param>
	/// <param name="useHex">
	/// Default = <c>false</c>. 
	/// If <c>true</c> the input may have 0-9 and A-F/a-f; Number will be checked against range as a hexadecimal value.
	/// If <c>false</c> the input may have 0-9 only; Number will be checked against range as a decimal value.
	/// </param>
	/// <seealso cref="FixedSizeInteger"/>
	/// <seealso cref="FractionalDecimal"/>
	public static BNF IntegerRange(long min, long max, bool allowLeadingWhitespace = false, bool useHex = false)
	{
		return new BNF(new VariableWidthIntegerRange(min, max, allowLeadingWhitespace, useHex));
	}

	/// <summary>
	/// Create a parser for a variable width signed decimal value.
	/// This can contain number separators, decimal points, and 'E notation'.
	/// <p/>
	/// By default, settings from <see cref="CultureInfo.CurrentCulture"/> will be used.
	/// Number separators are always optional, and specific positions are not enforced.
	/// 'E notation' is always case insensitive
	/// <p/>
	/// Does not allow leading or trailing decimal marks or grouping marks.
	/// </summary>
	/// <example>
	/// In 'invariant' culture: <code>
	/// +123,456,789.123E26
	/// -123456e10
	/// 123.04
	/// </code>
	/// With common European forms: <code>
	/// +123.456.789,123E26
	/// -123456e10
	/// 123,04
	/// </code>
	/// Using custom settings: <code>
	/// +123_456_789x123E26
	/// -123456e10
	/// 123x04
	/// </code>
	/// </example>
	/// <param name="groupMark">[Optional] Default = <see cref="CultureInfo.CurrentCulture"/>.NumberFormat.<see cref="NumberFormatInfo.NumberGroupSeparator"/>
	///     Acceptable number separator. Has no semantic meaning. This is allowed in any place in the input except the end, and can be repeated.
	///     An empty string as the group mark will disable the use of group marks.
	///     Must not contain characters 0-9.</param>
	/// <param name="decimalMark">[Optional] Default = <see cref="CultureInfo.CurrentCulture"/>.NumberFormat.<see cref="NumberFormatInfo.NumberDecimalSeparator"/>
	///     Acceptable decimal separator. This is allowed at most one time.
	///     Must not be empty, must not contain characters 0-9.</param>
	/// <param name="allowLeadingWhitespace">[Optional] Default = <c>false</c>.
	///     If <c>true</c> the input may have leading whitespace.
	///     If <c>false</c> the input must have digits in all places.
	/// </param>
	/// <param name="allowLoneDecimal">[Optional] Default = false.
	///     If true, the decimal place can be used without a number on either side, like <c>.9</c> or <c>1.</c></param>
	/// <param name="allowLeadingZero">[Optional] Default = false.
	///     If true, leading zeros are allowed, like <c>007</c> or <c>+0012</c>.
	///     A single <c>0</c>, or a zero before a decimal place is always allowed</param>
	/// <param name="allowLeadingPlus">[Optional] Default = true.
	///     If true, starting numbers with <c>+</c> is allowed, like <c>+5.0</c>.
	///     Otherwise, only starting with a negative or digit is allowed, like <c>-1</c> or <c>42</c></param>
	/// <seealso cref="FixedSizeInteger"/>
	/// <seealso cref="IntegerRange"/>
	/// <seealso href="https://learn.microsoft.com/en-us/dotnet/api/System.Globalization.CultureInfo.CurrentCulture"/>
	public static BNF FractionalDecimal(string? groupMark = null, string? decimalMark = null,
		bool allowLeadingWhitespace = false, bool allowLoneDecimal = false, bool allowLeadingZero = false, bool allowLeadingPlus = true)
	{
		var grp = groupMark ?? CultureInfo.CurrentCulture.NumberFormat.NumberGroupSeparator;
		var dec = decimalMark ?? CultureInfo.CurrentCulture.NumberFormat.NumberDecimalSeparator;
		if (dec == grp) throw new Exception("Group mark cannot be the same as decimal mark");
		if (grp.IndexOfAny(NumberCharacters) > -1) throw new Exception("Group mark cannot contain numbers");
		if (dec.IndexOfAny(NumberCharacters) > -1) throw new Exception("Decimal mark cannot contain numbers");

		return new BNF(new VariableWidthFractionalDecimal(grp, dec, allowLeadingWhitespace, allowLoneDecimal, allowLeadingZero, allowLeadingPlus));
	}

	/// <summary>
	/// Common string pattern for names and identifiers in programming languages and data formats.
	/// Must be at least 1 character. First character must be alphabetic.
	/// Subsequent characters may be alphanumeric.
	/// </summary>
	/// <param name="allowUnderscore">Default: true. If true, '_' is allowed at the start, end, or middle of the identifier</param>
	/// <param name="allowHyphen">Default: false. If true, '-' is allowed in the middle, but not the start or end, of the identifier</param>
	public static BNF IdentifierString(bool allowUnderscore = true, bool allowHyphen = false)
	{
		return new BNF(new IdentifierString(allowUnderscore, allowHyphen));
	}

	/// <summary>
	/// Match any length of string, upto but not including the terminating sub-string.
	/// If the terminator is not found, this fails to match.
	/// </summary>
	/// <param name="terminator">String that ends the match</param>
	public static BNF StringTerminatedBy(string terminator)
	{
		return new BNF(new StringTerminatedString(terminator));
	}
	#endregion Composite/combination helpers

	#region Common patterns and terminal helpers
	/// <summary>
	/// Match any one character
	/// </summary>
	public static BNF AnyChar => new(new AnyCharacter());

	/// <summary>
	/// Match any one character from the given UTF character category
	/// </summary>
	public static BNF UtfCategory(UnicodeCategory category)
	{
		return new BNF(new AnyCharacterInCategory(category));
	}

	/// <summary>
	/// Match any one character in any letter or a decimal digit Unicode category
	/// </summary>
	public static BNF AlphaNumeric => new(new CharacterPredicate(char.IsLetterOrDigit));

	/// <summary>
	/// Match any one character in the Unicode category 'UppercaseLetter'
	/// </summary>
	public static BNF Uppercase => new(new CharacterPredicate(char.IsUpper));

	/// <summary>
	/// Match any one character in the Unicode category 'LowercaseLetter'
	/// </summary>
	public static BNF Lowercase => new(new CharacterPredicate(char.IsLower));

	/// <summary>
	/// Match any one character in any letter Unicode category
	/// </summary>
	public static BNF Letter => new(new CharacterPredicate(char.IsLetter));

	/// <summary>
	/// Match any one character in any decimal digit Unicode category
	/// </summary>
	public static BNF Digit => new(new CharacterPredicate(char.IsDigit));

	/// <summary>
	/// Match any one character which is a hexadecimal digit
	/// </summary>
	public static BNF HexDigit => CharacterInRanges(('a','f'),('A','F'),('0','9'));

	/// <summary>
	/// Match an empty string
	/// </summary>
	public static BNF Empty => new(new EmptyMatch());
	
	/// <summary>
	/// Match the end of a line.
	/// This will match the longest of either <c>\r</c>, <c>\n</c>, or <c>\r\n</c>
	/// </summary>
	public static BNF LineEnd => new(new EndOfLine());
	
	/// <summary>
	/// Match anything except the end of a line
	/// </summary>
	public static BNF NotLineEnd => new(new LiteralCharacterSet('\r', '\n'));
	
	/// <summary>
	/// Match the end of input
	/// </summary>
	public static BNF EndOfInput => new(new EndOfInput());
	
	/// <summary>
	/// Match a single character of white-space
	/// </summary>
	public static BNF WhiteSpace => new(new Whitespace());

	/// <summary>
	/// Either the previous match ends in whitespace, or match a single character of white-space.
	/// </summary>
	public static BNF RequiredWhiteSpace => (PreviousEndsWith(WhiteSpace) | WhiteSpace);

	/// <summary>
	/// Match a range of white-space characters
	/// </summary>
	public static BNF WhiteSpaceCount(int min, int max) => new(new Whitespace(min, max));

	/// <summary>
	/// Match any number of white-space characters, or none
	/// </summary>
	public static BNF AnyWhiteSpace => new(new Whitespace(0, int.MaxValue));

	/// <summary>
	/// Match any number of white-space characters, at least 1
	/// </summary>
	public static BNF WhiteSpaceString => new(new Whitespace(1, int.MaxValue));

	#endregion Common patterns and terminal helpers

	#region Static values
	private static readonly char[] NumberCharacters = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' };

	/// <summary>
	/// Maximum value for a UTF character (limited by C# / dotnet)
	/// </summary>
	public const char MaxUtf = char.MaxValue;

	/// <summary>
	/// Minimum value for a printable character outside of the ASCII range
	/// </summary>
	public const char FirstNonAscii = '\u00A0';

	/// <summary>
	/// Left single quote <c>‘</c> ("smart quotes")
	/// </summary>
	public static BNF LeftSingleQuote => new LiteralCharacterSet('‘');
	/// <summary>
	/// Right single quote <c>’</c> ("smart quotes")
	/// </summary>
	public static BNF RightSingleQuote => new LiteralCharacterSet('’');

	/// <summary>
	/// Left single quote <c>“</c> ("smart quotes")
	/// </summary>
	public static BNF LeftDoubleQuote => new LiteralCharacterSet('“');
	/// <summary>
	/// Right single quote <c>”</c> ("smart quotes")
	/// </summary>
	public static BNF RightDoubleQuote => new LiteralCharacterSet('”');
	#endregion Static values

	#region Internal sub-types

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
		/// <b>Parsers should expect lowercase input</b>
		/// </summary>
		IgnoreCase = 2,
		
		/// <summary>
		/// Auto-advanced elements (like white-space skips)
		/// will be added to the result tree.
		/// </summary>
		IncludeSkippedElements = 4,
	}
	
	/// <summary>
	/// BNF structure, plus the correct scanner options
	/// </summary>
	public class Package
	{
		private readonly BNF      _bnf;
		private readonly Options  _options;

		/// <summary>
		/// [Optional] Parser used to skip insignificant patterns in the input.
		/// </summary>
		public IParser? AutoAdvance { get; }

		/// <summary>
		/// BNF structure, plus the correct scanner options
		/// </summary>
		internal Package(BNF bnf, Options options, IParser? autoAdvance)
		{
			_bnf = bnf;
			_options = options;
			AutoAdvance = autoAdvance;
		}

		/// <summary>
		/// Parse an input string, returning a match tree.
		/// <p/>
		/// This can return successful matches that consume only part of the input.
		/// To ensure that the entire input matches the parser, use <see cref="ParseEntireString"/>
		/// </summary>
		/// <param name="input">The string to parse</param>
		/// <param name="offset">Optional. Position in the input to start parsing</param>
		/// <param name="diagnostics">Optional (default: true). Record diagnostic info. Parsing is faster without</param>
		public ParserMatch ParsePartialString(string input, int offset = 0, bool diagnostics = true)
		{
			return _bnf.ParseString(input, offset, _options, AutoAdvance, diagnostics, mustConsumeAll: false);
		}

		/// <summary>
		/// Parse an input string, returning a match tree.
		/// This will return a failed match if it does not consume the entire input.
		/// <p/>
		/// To allow matches that use only part of the input, see <see cref="ParsePartialString"/>
		/// </summary>
		/// <param name="input">The string to parse</param>
		/// <param name="offset">Optional. Position in the input to start parsing</param>
		/// <param name="diagnostics">Optional (default: true). Record diagnostic info. Parsing is faster without</param>
		public ParserMatch ParseEntireString(string input, int offset = 0, bool diagnostics = true)
		{
			return _bnf.ParseString(input, offset, _options, AutoAdvance, diagnostics, mustConsumeAll: true);
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
			rec.Source = parser;
		}
	}

	/// <summary>
	/// A range of characters, inclusive of upper and lower
	/// </summary>
	public class CharacterRange
	{
		/// <summary>
		/// Create a character range
		/// </summary>
		private CharacterRange(char a, char b)
		{
			if (a < b)
			{
				Lower = a;
				Upper = b;
			}
			else
			{
				Lower = b;
				Upper = a;
			}
		}

		/// <summary>
		/// Implicitly cast a single character to a 1 character range
		/// </summary>
		public static implicit operator CharacterRange(char c)
		{
			return new CharacterRange(c,c);
		}

		/// <summary>
		/// Implicitly cast a single character to a 1 character range
		/// </summary>
		public static implicit operator CharacterRange(int c)
		{
			return new CharacterRange((char)c,(char)c);
		}

		/// <summary>
		/// Implicitly cast a tuple with an upper and lower character to a range
		/// </summary>
		public static implicit operator CharacterRange(ValueTuple<char,char> t)
		{
			return new CharacterRange(t.Item1, t.Item2);
		}

		/// <summary>
		/// Implicitly cast a tuple with an upper and lower character to a range
		/// </summary>
		public static implicit operator CharacterRange(ValueTuple<int,int> t)
		{
			return new CharacterRange((char)t.Item1, (char)t.Item2);
		}

		/// <summary>
		/// Return the lowest-ordered character that is considered a match
		/// </summary>
		public readonly char Lower;

		/// <summary>
		/// Return the highest-ordered character that is considered a match
		/// </summary>
		public readonly char Upper;

		/// <inheritdoc />
		public override string ToString()
		{
			if (Lower == Upper) return Upper.ToString();
			return Lower + "-" + Upper;
		}
	}
	#endregion Internal sub-types

	#region IParser pass-through

	private bool _autoAdvanceEnabled = true;
	private bool _atomic;

	/// <summary>
	/// Internal reference to the real parser instance
	/// </summary>
	private IParser _parserTree;

	/// <summary>
	/// Create a BNF wrapper for an <see cref="IParser"/> instance
	/// </summary>
	internal BNF(IParser parserTree)
	{
		_parserTree = parserTree;
	}

	/// <inheritdoc />
	public ParserMatch Parse(IScanner scan, ParserMatch? previousMatch = null, bool allowAutoAdvance = true)
	{
		var result = _parserTree.Parse(scan, previousMatch, _autoAdvanceEnabled && allowAutoAdvance);
		return _atomic ? result.Compact() : result;
	}

	/// <inheritdoc />
	public IEnumerable<IParser> ChildParsers() => _parserTree.ChildParsers();

	/// <inheritdoc />
	public bool IsOptional() => _parserTree.IsOptional();

	/// <inheritdoc />
	public string? Tag
	{
		get => _parserTree.Tag;
		set => _parserTree.Tag = value;
	}

	/// <inheritdoc />
	public ScopeType Scope
	{
		get => _parserTree.Scope;
		set => _parserTree.Scope = value;
	}

	/// <inheritdoc />
	public bool HasMetaData()
	{
		return _parserTree.HasMetaData();
	}

	/// <inheritdoc />
	public string ShortDescription(int depth)
	{
		return _parserTree.ShortDescription(depth);
	}

	/// <summary>
	/// Try to match scanner data against the contained parser
	/// </summary>
	// ReSharper disable once UnusedMember.Global
	internal ParserMatch TryMatch(IScanner scan, ParserMatch? previousMatch, bool allowAutoAdvance)
	{
		if (_parserTree is not Parser imp) throw new Exception($"Invalid parser tree: expected '{nameof(Parser)}', got '{_parserTree.GetType().Name}'");
		return imp.TryMatch(scan, previousMatch, allowAutoAdvance);
	}

	/// <inheritdoc />
	public override string ToString()
	{
		return _parserTree.ToString();
	}

	#endregion IParser pass-through

}