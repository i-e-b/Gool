using System.Collections.Generic;
using Gool.Results;

namespace Gool.Scanners;

/// <summary>
/// Scanners provide an interface to the input stream to be parsed
/// </summary>
public interface IScanningDiagnostics {
	/// <summary>
	/// Set a point at which a parser failed
	/// </summary>
	void AddFailure(ParserMatch failMatch);

	/// <summary>
	/// Add a success path, for diagnostic use
	/// </summary>
	void AddSuccess(ParserMatch newMatch);

	/// <summary>
	/// Output a list of all fail points since the last success.
	/// This is intended for lower-level diagnostics.
	/// For parser output to show users, see <see cref="IScanner.FurthestMatch"/>
	/// </summary>
	List<string> ListFailures(int minimumOffset = 0, bool showDetails = false);

	/// <summary>
	/// Mark this scanner as having been used
	/// </summary>
	void Complete();
}