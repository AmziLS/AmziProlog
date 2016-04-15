using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;

namespace pets_callback
{
	/// <summary>
	/// Summary description for InputDialog.
	/// </summary>
	public class InputDialog : System.Windows.Forms.Form
	{
		private System.Windows.Forms.Label PromptLabel;
		private System.Windows.Forms.TextBox InputBox;
		private System.Windows.Forms.Button OKButton;
		/// <summary>
		/// Required designer variable.
		/// </summary>
		private System.ComponentModel.Container components = null;

		public InputDialog(string prompt)
		{
			//
			// Required for Windows Form Designer support
			//
			InitializeComponent();

			//
			// TODO: Add any constructor code after InitializeComponent call
			//
			this.PromptLabel.Text = prompt;
		}

		/// <summary>
		/// Clean up any resources being used.
		/// </summary>
		protected override void Dispose( bool disposing )
		{
			if( disposing )
			{
				if(components != null)
				{
					components.Dispose();
				}
			}
			base.Dispose( disposing );
		}

		#region Windows Form Designer generated code
		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
			this.PromptLabel = new System.Windows.Forms.Label();
			this.InputBox = new System.Windows.Forms.TextBox();
			this.OKButton = new System.Windows.Forms.Button();
			this.SuspendLayout();
			// 
			// PromptLabel
			// 
			this.PromptLabel.Location = new System.Drawing.Point(16, 16);
			this.PromptLabel.Name = "PromptLabel";
			this.PromptLabel.Size = new System.Drawing.Size(256, 88);
			this.PromptLabel.TabIndex = 0;
			// 
			// InputBox
			// 
			this.InputBox.Location = new System.Drawing.Point(8, 128);
			this.InputBox.Name = "InputBox";
			this.InputBox.Size = new System.Drawing.Size(264, 20);
			this.InputBox.TabIndex = 1;
			this.InputBox.Text = "";
			// 
			// OKButton
			// 
			this.OKButton.Location = new System.Drawing.Point(8, 168);
			this.OKButton.Name = "OKButton";
			this.OKButton.Size = new System.Drawing.Size(56, 24);
			this.OKButton.TabIndex = 2;
			this.OKButton.Text = "OK";
			this.OKButton.Click += new System.EventHandler(this.OKButton_Click);
			// 
			// InputDialog
			// 
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
			this.ClientSize = new System.Drawing.Size(290, 207);
			this.ControlBox = false;
			this.Controls.Add(this.OKButton);
			this.Controls.Add(this.InputBox);
			this.Controls.Add(this.PromptLabel);
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.MaximizeBox = false;
			this.MinimizeBox = false;
			this.Name = "InputDialog";
			this.Text = "Extended Predicate: prompt";
			this.ResumeLayout(false);

		}
		#endregion

		private void OKButton_Click(object sender, System.EventArgs e)
		{
			this.DialogResult = DialogResult.OK;
		}

		public string GetInput()
		{
			return this.InputBox.Text;
		}
	}
}
