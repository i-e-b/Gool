namespace Phantom.Scanners {
	public class TransformToLower: ITransform {
		/// <summary>
		/// Convert irregular cased input to lowercased input
		/// </summary>
		string ITransform.Transform(string s) {
			return s.ToLower();
		}

		/// <summary>
		/// Convert irregular cased input to lowercased input
		/// </summary>
		char ITransform.Transform(char c) {
			return c.ToString().ToLower()[0];
		}
	}
}
