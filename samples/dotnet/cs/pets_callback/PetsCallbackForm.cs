using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using System.Data;

namespace pets_callback
{
	/// <summary>
	/// Summary description for PetsCallbackForm.
	/// </summary>
	public class PetsCallbackForm : System.Windows.Forms.Form
	{
		private System.Windows.Forms.TextBox PetText;
		private System.Windows.Forms.Button GoButton;
		/// <summary>
		/// Required designer variable.
		/// </summary>
		private System.ComponentModel.Container components = null;

		public PetsCallbackForm()
		{
			//
			// Required for Windows Form Designer support
			//
			InitializeComponent();

			//
			// TODO: Add any constructor code after InitializeComponent call
			//
		}

		/// <summary>
		/// Clean up any resources being used.
		/// </summary>
		protected override void Dispose( bool disposing )
		{
			if( disposing )
			{
				if (components != null) 
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
			this.PetText = new System.Windows.Forms.TextBox();
			this.GoButton = new System.Windows.Forms.Button();
			this.SuspendLayout();
			// 
			// PetText
			// 
			this.PetText.Location = new System.Drawing.Point(96, 88);
			this.PetText.Name = "PetText";
			this.PetText.Size = new System.Drawing.Size(104, 20);
			this.PetText.TabIndex = 0;
			this.PetText.Text = "";
			// 
			// GoButton
			// 
			this.GoButton.Location = new System.Drawing.Point(96, 32);
			this.GoButton.Name = "GoButton";
			this.GoButton.Size = new System.Drawing.Size(104, 40);
			this.GoButton.TabIndex = 1;
			this.GoButton.Text = "Get Pet";
			this.GoButton.Click += new System.EventHandler(this.GoButton_Click);
			// 
			// PetsCallbackForm
			// 
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
			this.ClientSize = new System.Drawing.Size(292, 273);
			this.Controls.Add(this.GoButton);
			this.Controls.Add(this.PetText);
			this.Name = "PetsCallbackForm";
			this.Text = "Pets Callback";
			this.ResumeLayout(false);

		}
		#endregion

		/// <summary>
		/// The main entry point for the application.
		/// </summary>
		[STAThread]
		static void Main() 
		{
			Application.Run(new PetsCallbackForm());
		}

		private void GoButton_Click(object sender, System.EventArgs e)
		{
			LogicServer ls;
			PrologPredicate prompt_pred;
			int term;
			string pet;

			ls = new LogicServer();
			prompt_pred = new PrologPredicate(prompt);
			try 
			{
				ls.Init("");
				ls.AddPred("prompt", 2, prompt_pred);
				ls.Load("pets.xpl");
				term = ls.ExecStr("pet(X)");
            if (term == 0) 
            {
               MessageBox.Show("Sound not found", "ERROR", 
                  MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
            else 
            {
               pet = ls.GetStrArg(term, 1);
               PetText.Text = pet;
            }
				ls.Close();
			} 
			catch (LSException ex) 
			{
				MessageBox.Show(ex.GetMessage(), "ERROR", 
					MessageBoxButtons.OK, MessageBoxIcon.Error);
			}
		
		}

		bool prompt(LogicServer ls) 
		{
			string prompt_text, answer;
			InputDialog dlg;
			
			prompt_text = ls.GetStrParm(1);
			dlg = new InputDialog(prompt_text);
			dlg.ShowDialog();

			answer = dlg.GetInput();
			if (ls.UnifyStrParm(2, answer) == 0) return false;
			return true;
		}
	}
}
