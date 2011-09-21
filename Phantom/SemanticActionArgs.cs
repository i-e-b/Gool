using System;
using System.Collections.Generic;
using System.Text;

namespace Phantom {
	/// <summary>
	/// Template for Semantic Action event handler
	/// </summary>
	/// <param name="sender">Object sending the message</param>
	/// <param name="args">Parser match details</param>
	//public delegate void ActionHandler(object sender, SemanticActionArgs args);

	public class SemanticActionArgs : EventArgs {
		private Parsers.ParserMatch m_Match;
		private Object m_TypeValue;

		/// <summary>
		/// Create a new untyped semantic action event argument
		/// </summary>
		/// <param name="match">The parser match that triggered the event</param>
		public SemanticActionArgs(Parsers.ParserMatch match) {
			if (match == null)
				throw new ArgumentNullException("Can't create an event from a null match.");
			m_Match = match;
			m_TypeValue = null;
		}

		/// <summary>
		/// Create a new semantic action event argument with boxed typed data.
		/// </summary>
		/// <param name="match">The parser match that triggered the event</param>
		/// <param name="typedValue">Converted type data (boxed inside an object)</param>
		public SemanticActionArgs(Parsers.ParserMatch match, object typedValue) {
			if (match == null)
				throw new ArgumentNullException("Can't create an event from a null match.");
			if (typedValue == null)
				throw new ArgumentNullException("Typed data was null in SemanticActionArgs.");
			m_Match = match;
			m_TypeValue = typedValue;
		}

		/// <summary>
		/// The parser match that triggered the event
		/// </summary>
		public Parsers.ParserMatch Match {
			get {
				return m_Match;
			}
		}

		/// <summary>
		/// Textual value of parser match
		/// </summary>
		public String Value {
			get {
				return Match.Value;
			}
		}

		/// <summary>
		/// Typed, boxed value of parser match
		/// </summary>
		public Object TypeValue {
			get {
				return m_TypeValue;
			}
		}
	}
}
