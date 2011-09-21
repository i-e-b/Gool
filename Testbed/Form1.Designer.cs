namespace Testbed {
	partial class Form1 {
		/// <summary>
		/// Required designer variable.
		/// </summary>
		private System.ComponentModel.IContainer components = null;

		/// <summary>
		/// Clean up any resources being used.
		/// </summary>
		/// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
		protected override void Dispose(bool disposing) {
			if (disposing && (components != null)) {
				components.Dispose();
			}
			base.Dispose(disposing);
		}

		#region Windows Form Designer generated code

		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent() {
			this.tableLayoutPanel1 = new System.Windows.Forms.TableLayoutPanel();
			this.BTN_Parse = new System.Windows.Forms.Button();
			this.splitContainer1 = new System.Windows.Forms.SplitContainer();
			this.inputBox = new System.Windows.Forms.RichTextBox();
			this.splitContainer2 = new System.Windows.Forms.SplitContainer();
			this.resultsBox = new System.Windows.Forms.RichTextBox();
			this.feedBox = new System.Windows.Forms.RichTextBox();
			this.tableLayoutPanel1.SuspendLayout();
			this.splitContainer1.Panel1.SuspendLayout();
			this.splitContainer1.Panel2.SuspendLayout();
			this.splitContainer1.SuspendLayout();
			this.splitContainer2.Panel1.SuspendLayout();
			this.splitContainer2.Panel2.SuspendLayout();
			this.splitContainer2.SuspendLayout();
			this.SuspendLayout();
			// 
			// tableLayoutPanel1
			// 
			this.tableLayoutPanel1.ColumnCount = 1;
			this.tableLayoutPanel1.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 100F));
			this.tableLayoutPanel1.Controls.Add(this.BTN_Parse, 0, 1);
			this.tableLayoutPanel1.Controls.Add(this.splitContainer1, 0, 0);
			this.tableLayoutPanel1.Dock = System.Windows.Forms.DockStyle.Fill;
			this.tableLayoutPanel1.Location = new System.Drawing.Point(0, 0);
			this.tableLayoutPanel1.Name = "tableLayoutPanel1";
			this.tableLayoutPanel1.RowCount = 2;
			this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 100F));
			this.tableLayoutPanel1.RowStyles.Add(new System.Windows.Forms.RowStyle());
			this.tableLayoutPanel1.Size = new System.Drawing.Size(597, 375);
			this.tableLayoutPanel1.TabIndex = 0;
			// 
			// BTN_Parse
			// 
			this.BTN_Parse.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
			this.BTN_Parse.Location = new System.Drawing.Point(519, 349);
			this.BTN_Parse.Name = "BTN_Parse";
			this.BTN_Parse.Size = new System.Drawing.Size(75, 23);
			this.BTN_Parse.TabIndex = 0;
			this.BTN_Parse.Text = "Parse";
			this.BTN_Parse.UseVisualStyleBackColor = true;
			this.BTN_Parse.Click += new System.EventHandler(this.BTN_Parse_Click);
			// 
			// splitContainer1
			// 
			this.splitContainer1.Dock = System.Windows.Forms.DockStyle.Fill;
			this.splitContainer1.Location = new System.Drawing.Point(3, 3);
			this.splitContainer1.Name = "splitContainer1";
			// 
			// splitContainer1.Panel1
			// 
			this.splitContainer1.Panel1.Controls.Add(this.inputBox);
			// 
			// splitContainer1.Panel2
			// 
			this.splitContainer1.Panel2.Controls.Add(this.splitContainer2);
			this.splitContainer1.Size = new System.Drawing.Size(591, 340);
			this.splitContainer1.SplitterDistance = 283;
			this.splitContainer1.TabIndex = 1;
			// 
			// inputBox
			// 
			this.inputBox.DetectUrls = false;
			this.inputBox.Dock = System.Windows.Forms.DockStyle.Fill;
			this.inputBox.Font = new System.Drawing.Font("Courier New", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
			this.inputBox.HideSelection = false;
			this.inputBox.Location = new System.Drawing.Point(0, 0);
			this.inputBox.Name = "inputBox";
			this.inputBox.Size = new System.Drawing.Size(283, 340);
			this.inputBox.TabIndex = 0;
			this.inputBox.Text = "<root_tag>\n<innerTag attrib=\"x\">some text</innerTag>\nMore text\n</root_tag>";
			// 
			// splitContainer2
			// 
			this.splitContainer2.Dock = System.Windows.Forms.DockStyle.Fill;
			this.splitContainer2.Location = new System.Drawing.Point(0, 0);
			this.splitContainer2.Name = "splitContainer2";
			this.splitContainer2.Orientation = System.Windows.Forms.Orientation.Horizontal;
			// 
			// splitContainer2.Panel1
			// 
			this.splitContainer2.Panel1.Controls.Add(this.resultsBox);
			// 
			// splitContainer2.Panel2
			// 
			this.splitContainer2.Panel2.Controls.Add(this.feedBox);
			this.splitContainer2.Size = new System.Drawing.Size(304, 340);
			this.splitContainer2.SplitterDistance = 147;
			this.splitContainer2.TabIndex = 1;
			// 
			// resultsBox
			// 
			this.resultsBox.Dock = System.Windows.Forms.DockStyle.Fill;
			this.resultsBox.Font = new System.Drawing.Font("Courier New", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
			this.resultsBox.Location = new System.Drawing.Point(0, 0);
			this.resultsBox.Name = "resultsBox";
			this.resultsBox.Size = new System.Drawing.Size(304, 147);
			this.resultsBox.TabIndex = 0;
			this.resultsBox.Text = "Results Box";
			// 
			// feedBox
			// 
			this.feedBox.Dock = System.Windows.Forms.DockStyle.Fill;
			this.feedBox.Font = new System.Drawing.Font("Courier New", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
			this.feedBox.Location = new System.Drawing.Point(0, 0);
			this.feedBox.Name = "feedBox";
			this.feedBox.Size = new System.Drawing.Size(304, 189);
			this.feedBox.TabIndex = 0;
			this.feedBox.Text = "Action feed";
			// 
			// Form1
			// 
			this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
			this.ClientSize = new System.Drawing.Size(597, 375);
			this.Controls.Add(this.tableLayoutPanel1);
			this.Name = "Form1";
			this.Text = "Testbed";
			this.tableLayoutPanel1.ResumeLayout(false);
			this.splitContainer1.Panel1.ResumeLayout(false);
			this.splitContainer1.Panel2.ResumeLayout(false);
			this.splitContainer1.ResumeLayout(false);
			this.splitContainer2.Panel1.ResumeLayout(false);
			this.splitContainer2.Panel2.ResumeLayout(false);
			this.splitContainer2.ResumeLayout(false);
			this.ResumeLayout(false);

		}

		#endregion

		private System.Windows.Forms.TableLayoutPanel tableLayoutPanel1;
		private System.Windows.Forms.Button BTN_Parse;
		private System.Windows.Forms.SplitContainer splitContainer1;
		private System.Windows.Forms.RichTextBox resultsBox;
		private System.Windows.Forms.RichTextBox inputBox;
		private System.Windows.Forms.SplitContainer splitContainer2;
		private System.Windows.Forms.RichTextBox feedBox;
	}
}

