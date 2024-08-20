namespace Phantom.Scanners;

/// <summary>
/// A transform that represents no change to the input
/// </summary>
public class NoTransform : ITransform
{
	/// <inheritdoc />
	public string Transform(string s) => s;
}