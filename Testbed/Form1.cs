using System;
using System.Collections.Generic;
using System.Windows.Forms;
using Phantom;
using Phantom.Scanners;
using Phantom.Parsers;

namespace Testbed {
	public partial class Form1 : Form {
		public Form1() {
			InitializeComponent();
		}

		private void BTN_Parse_Click(object sender, EventArgs e) {
			feedBox.Clear();
			string input = inputBox.Text;

			Parser eof = new Phantom.Parsers.Terminals.EndOfInput();

			ScanStrings scanner = new ScanStrings(input);
			scanner.SkipWhitespace = true;
			scanner.Transform = new TransformToLower();
			XMLParser pp = new XMLParser(); //new PascalParser();
			Parser r1 = pp.TheParser > eof;

			//Parser r1 = (Parser)"hello" > "," > "world" > "!" > eof; // dead simple, matches "Hello, world!";

			r1.ScannerInput = scanner;

			ParserMatch m = r1.Compare();

			if (m.Success) {
				resultsBox.Text = m.ToString();
				// grab all bottom matches:
				List<ParserMatch> matches = new List<ParserMatch>(m.BottomLevelMatches());

				foreach (ParserMatch chunk in matches) {
					feedBox.Text += chunk.Value + "\r\n";
				}
			} else {
				resultsBox.Text = "*NO MATCH*\r\n" + scanner.BadPatch(5);
			}


			/*foreach (string s in pp.ResultList) {
				feedBox.Text += s + "\r\n";
			}*/
		}

		public void gt_action(Object sender, SemanticActionArgs args) {
			//MessageBox.Show("Action fired on \"" + args.Value +"\"");
			feedBox.Text += args.Value + "\r\n";
		}
	}
}