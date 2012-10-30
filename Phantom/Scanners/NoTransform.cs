namespace Phantom.Scanners
{
	public class NoTransform : ITransform
	{
		public string Transform(string s)
		{
			return s;
		}

		public char Transform(char c)
		{
			return c;
		}
	}
}