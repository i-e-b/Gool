using System;
using System.Text.RegularExpressions;
using Phantom.Parsers;
using Phantom.Parsers.Composite;
using Phantom.Parsers.Terminals;

namespace Phantom
{
	/// <summary>
	/// Provides a BNF-like syntax for building parsers
	/// </summary>
	public abstract class BNF: Parser, IParser
	{
		/*
		
		
		// There are a few changes compared to Spirit, all due to overloading
		// restrictions in C#.
		//   1) >> replaced with >			(C# needs one operand of >> to be an integer)
		//   2) * replaced with -			(C# has no pointer math, so no unary * )

		/// <summary>
		/// Convert a LiteralString parser into a Regular expression parser, using the given options.
		/// </summary>
		/// <param name="opts">Regular expression options for the converted parser to use.</param>
		/// <returns>Converted RegularExpression parser</returns>
		public BNF this[RegexOptions opts]
		{
			get
			{
				if (this is LiteralString)
				{
					var incoming = (LiteralString) this;
					return new RegularExpression(incoming.MatchLiteral, opts);
				}
				if (this is RegularExpression)
				{
					return this;
				}
				throw new ArgumentException("Tried to convert a non-string terminal into a Regular Expression");
			}
		}


		/// <summary>
		/// Convert a character into a parser
		/// </summary>
		/// <remarks>
		/// This is a lovely feature of C#. It makes a lot of redundant code disappear,
		/// and lets us get away with linguistic murder!
		/// Whoever made this part of the language, I salute you!
		/// </remarks>
		public static implicit operator Parser(char c)
		{
			return new LiteralCharacter(c);
		}

		/// <summary>
		/// Convert a string into a parser.
		/// If the string starts with '#' it will be treated as a regular expression
		/// unless it also starts with '##'. If the string starts with '#', that
		/// character will be removed.
		/// </summary>
		public static implicit operator Parser(string s)
		{
			string pattern;
			if (s.StartsWith("#"))
			{
				pattern = s.Substring(1);
				if (!pattern.StartsWith("#"))
				{
					return new RegularExpression(pattern);
				}
			}
			else
			{
				pattern = s;
			}
			return new LiteralString(pattern);
		}


		/// <summary>
		/// Create a sequential parser that matches _a_ then _b_
		/// </summary>
		public static Parser operator >(Parser a, Parser b)
		{
			if (a == null)
				throw new ArgumentNullException("a", "Left side of sequence parser is null");
			if (b == null)
				throw new ArgumentNullException("b", "Right side of sequence parser is null");

			return new Sequence(a, b);
		}

		/// <summary>
		/// Create a loop parser that matches a list of _a_, each being terminated by _b_
		/// The last item _a_ may be terminated, but need not be.
		/// </summary>
		public static Parser operator <(Parser a, Parser b)
		{
			if (a == null)
				throw new ArgumentNullException("a", "Left side of list parser is null");
			if (b == null)
				throw new ArgumentNullException("b", "Right side of list parser is null");

			return new TerminatedList(a, b);
		}


		/// <summary>
		/// Create a loop parser that matches zero or more _a_
		/// </summary>
		public static Parser operator -(Parser a)
		{
			if (a == null)
				throw new ArgumentNullException("a", "Loop parser is null");

			return new Repetition(a, 0, uint.MaxValue);
		}

		/// <summary>
		/// Create a loop parser that matches one or more _a_
		/// </summary>
		public static Parser operator +(Parser a)
		{
			if (a == null)
				throw new ArgumentNullException("a", "Loop parser is null");

			return new Repetition(a, 1, uint.MaxValue);
		}

		/// <summary>
		/// Create a optional parser that matches zero or one _a_
		/// </summary>
		public static Parser operator !(Parser a)
		{
			if (a == null)
				throw new ArgumentNullException("a", "Option parser is null");

			return new Repetition(a, 0, 1);
		}

		/// <summary>
		/// Create a loop parser that matches a list of _a_, delimited by _b_
		/// </summary>
		public static Parser operator %(Parser a, Parser b)
		{
			if (a == null)
				throw new ArgumentNullException("a", "Left side of list parser is null");
			if (b == null)
				throw new ArgumentNullException("b", "Right side of list parser is null");

			return new DelimitedList(a, b);
		}


		/// <summary>
		/// Create a Union/Alternative parser that matches _a_ or _b_
		/// </summary>
		public static Parser operator |(Parser a, Parser b)
		{
			if (a == null)
				throw new ArgumentNullException("a", "Left side of union parser is null");
			if (b == null)
				throw new ArgumentNullException("b", "Right side of union parser is null");

			return new Union(a, b);
		}

		/// <summary>
		/// Create an Intersection parser that matches both _a_ and _b_
		/// </summary>
		public static Parser operator &(Parser a, Parser b)
		{
			if (a == null)
				throw new ArgumentNullException("a", "Left side of intersection parser is null");
			if (b == null)
				throw new ArgumentNullException("b", "Right side of intersection parser is null");

			return new Intersection(a, b);
		}

		/// <summary>
		/// Create an Exclusive-Or parser that matches _a_ or _b_ but not both
		/// </summary>
		public static Parser operator ^(Parser a, Parser b)
		{
			if (a == null)
				throw new ArgumentNullException("a", "Left side of Exclusive-Or parser is null");
			if (b == null)
				throw new ArgumentNullException("b", "Right side of Exclusive-Or parser is null");

			return new Exclusive(a, b);
		}

		
		*/
	}
}
