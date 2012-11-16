using System;
using System.Text.RegularExpressions;
using Phantom.Parsers;
using Phantom.Parsers.Composite;
using Phantom.Parsers.Terminals;

namespace Phantom
{
	/// <summary>
	/// Provides a BNF-like syntax for building parsers.<br/>
	/// &lt; terminated list; &gt; sequence;
	/// - zero or more; + one or more; ! zero or one;
	/// %  delimited list; | union; &amp; intersection;
	/// ^ xor; "#..." regex; '.' lit char; "..." lit string.
	/// </summary>
	public class BNF
	{
		// Inspired by Spirit parser http://boost-spirit.com/home/
		// There are a few changes compared to Spirit, all due to overloading
		// restrictions in C#.
		//   1) >> replaced with >			(C# needs one operand of >> to be an integer)
		//   2) * replaced with -			(C# has no normal pointer math, so no unary * )

		private readonly IParser parserTree;

		public BNF(IParser parserTree)
		{
			this.parserTree = parserTree;
		}

		/// <summary>
		/// Regular expression options passed to a regexes build with BNF
		/// </summary>
		public static RegexOptions RegexOptions { get; set; }

		/// <summary>
		/// Parser resulting from the BNF syntax
		/// </summary>
		/// <returns></returns>
		public IParser Result()
		{
			return parserTree;
		}

		/// <summary>
		/// Create a self-recursive parser structure
		/// </summary>
		public static BNF Recursive(Func<BNF, BNF> ParserTreeFunction)
		{
			// The way this works involves a lot of bad-practice and hidden typecasts.
			var hold = new Recursion();

			var src = ParserTreeFunction(hold);
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
		/// Convert a string into a parser.
		/// If the string starts with '#' it will be treated as a regular expression
		/// unless it also starts with '##'. If the string starts with '#', that
		/// character will be removed.
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
		/// Create a sequential parser that matches _a_ then _b_
		/// </summary>
		public static BNF operator >(BNF a, BNF b)
		{
			if (a == null)
				throw new ArgumentNullException("a", "Left side of sequence parser is null");
			if (b == null)
				throw new ArgumentNullException("b", "Right side of sequence parser is null");

			return new BNF(new Sequence(a.Result(), b.Result()));
		}

		/// <summary>
		/// Create a loop parser that matches a list of _a_, each being terminated by _b_
		/// The last item _a_ may be terminated, but need not be.
		/// </summary>
		public static BNF operator <(BNF a, BNF b)
		{
			if (a == null)
				throw new ArgumentNullException("a", "Left side of list parser is null");
			if (b == null)
				throw new ArgumentNullException("b", "Right side of list parser is null");

			return new BNF(new TerminatedList(a.Result(), b.Result()));
		}


		/// <summary>
		/// Create a loop parser that matches zero or more _a_
		/// </summary>
		public static BNF operator -(BNF a)
		{
			if (a == null)
				throw new ArgumentNullException("a", "Loop parser is null");

			return new BNF(new Repetition(a.Result(), 0, uint.MaxValue));
		}

		/// <summary>
		/// Create a loop parser that matches one or more _a_
		/// </summary>
		public static BNF operator +(BNF a)
		{
			if (a == null)
				throw new ArgumentNullException("a", "Loop parser is null");

			return new BNF(new Repetition(a.Result(), 1, uint.MaxValue));
		}

		/// <summary>
		/// Create a optional parser that matches zero or one _a_
		/// </summary>
		public static BNF operator !(BNF a)
		{
			if (a == null)
				throw new ArgumentNullException("a", "Option parser is null");

			return new BNF(new Repetition(a.Result(), 0, 1));
		}

		/// <summary>
		/// Create a loop parser that matches a list of _a_, delimited by _b_
		/// </summary>
		public static BNF operator %(BNF a, BNF b)
		{
			if (a == null)
				throw new ArgumentNullException("a", "Left side of list parser is null");
			if (b == null)
				throw new ArgumentNullException("b", "Right side of list parser is null");

			return new BNF(new DelimitedList(a.Result(), b.Result()));
		}

		/// <summary>
		/// Create a Union/Alternative parser that matches _a_ or _b_
		/// </summary>
		public static BNF operator |(BNF a, BNF b)
		{
			if (a == null)
				throw new ArgumentNullException("a", "Left side of union parser is null");
			if (b == null)
				throw new ArgumentNullException("b", "Right side of union parser is null");

			return new BNF(new Union(a.Result(), b.Result()));
		}

		/// <summary>
		/// Create an Intersection parser that matches both _a_ and _b_
		/// </summary>
		public static BNF operator &(BNF a, BNF b)
		{
			if (a == null)
				throw new ArgumentNullException("a", "Left side of intersection parser is null");
			if (b == null)
				throw new ArgumentNullException("b", "Right side of intersection parser is null");

			return new BNF(new Intersection(a.Result(), b.Result()));
		}

		/// <summary>
		/// Create an Exclusive-Or parser that matches _a_ or _b_ but not both
		/// </summary>
		public static BNF operator ^(BNF a, BNF b)
		{
			if (a == null)
				throw new ArgumentNullException("a", "Left side of Exclusive-Or parser is null");
			if (b == null)
				throw new ArgumentNullException("b", "Right side of Exclusive-Or parser is null");

			return new BNF(new Exclusive(a.Result(), b.Result()));
		}
	}
}
