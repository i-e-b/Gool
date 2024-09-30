using System.Collections.Generic;
using Gool.Parsers;
using Gool.Results;

namespace Gool.Scanners;

/// <summary>
/// Scanners provide an interface to the input stream to be parsed
/// </summary>
public interface IScanningDiagnostics {
	/// <summary>
	/// Set a point at which a parser failed
	/// </summary>
	void AddFailure(IParser failedParser, ParserMatch? previousMatch);

	/// <summary>
	/// Output a list of all fail points since the last success.
	/// This is intended for lower-level diagnostics.
	/// For parser output to show users, see <see cref="IScanner.FurthestMatch"/>
	/// </summary>
	List<string> ListFailures(bool includePartialMatches = false);

	/// <summary>
	/// Clear the stored list of failures. Should be called whenever a parser succeeds
	/// </summary>
	void ClearFailures();

	/// <summary>
	/// Returns a sample string from after the scanner stops.
	/// </summary>
	string BadPatch(int length);
	
	/// <summary>
	/// Mark this scanner as having been used
	/// </summary>
	void Complete();
}