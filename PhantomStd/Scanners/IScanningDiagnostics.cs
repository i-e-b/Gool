using System.Collections.Generic;
using Phantom.Results;

namespace Phantom.Scanners;

/// <summary>
/// Scanners provide an interface to the input stream to be parsed
/// </summary>
public interface IScanningDiagnostics {

	/// <summary>
	/// Returns a string of the match that had the highest value of (offset+length)
	/// </summary>
	string? FurthestMatch();

	/// <summary>
	/// Set a point at which a parser failed
	/// </summary>
	void AddFailure(IParser failedParser, ParserMatch? previousMatch);

	/// <summary>
	/// Output a list of all fail points since the last success.
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
}