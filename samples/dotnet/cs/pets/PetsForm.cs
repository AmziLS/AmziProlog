using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using System.Data;

namespace pets
{
	/// <summary>
	/// Summary description for Form1.
	/// </summary>
	public class PetsForm : System.Windows.Forms.Form
	{
		private System.Windows.Forms.Label label1;
		private System.Windows.Forms.TextBox SoundText;
		private System.Windows.Forms.Label label2;
		private System.Windows.Forms.TextBox PetText;
		private System.Windows.Forms.Button GetPetButton;
		/// <summary>
		/// Required designer variable.
		/// </summary>
		private System.ComponentModel.Container components = null;

		public PetsForm()
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(PetsForm));
            this.SoundText = new System.Windows.Forms.TextBox();
            this.label1 = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            this.PetText = new System.Windows.Forms.TextBox();
            this.GetPetButton = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // SoundText
            // 
            this.SoundText.Location = new System.Drawing.Point(112, 16);
            this.SoundText.Name = "SoundText";
            this.SoundText.Size = new System.Drawing.Size(88, 20);
            this.SoundText.TabIndex = 0;
            // 
            // label1
            // 
            this.label1.Location = new System.Drawing.Point(8, 16);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(72, 16);
            this.label1.TabIndex = 1;
            this.label1.Text = "Enter Sound:";
            // 
            // label2
            // 
            this.label2.Location = new System.Drawing.Point(8, 48);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(88, 16);
            this.label2.TabIndex = 2;
            this.label2.Text = "Your Pet is a:";
            // 
            // PetText
            // 
            this.PetText.Location = new System.Drawing.Point(112, 48);
            this.PetText.Name = "PetText";
            this.PetText.Size = new System.Drawing.Size(88, 20);
            this.PetText.TabIndex = 3;
            // 
            // GetPetButton
            // 
            this.GetPetButton.Location = new System.Drawing.Point(216, 16);
            this.GetPetButton.Name = "GetPetButton";
            this.GetPetButton.Size = new System.Drawing.Size(64, 32);
            this.GetPetButton.TabIndex = 4;
            this.GetPetButton.Text = "Get Pet";
            this.GetPetButton.Click += new System.EventHandler(this.GetPetButton_Click);
            // 
            // PetsForm
            // 
            this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
            this.ClientSize = new System.Drawing.Size(292, 273);
            this.Controls.Add(this.GetPetButton);
            this.Controls.Add(this.PetText);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.SoundText);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Name = "PetsForm";
            this.Text = "Pets";
            this.ResumeLayout(false);
            this.PerformLayout();

		}
		#endregion

		/// <summary>
		/// The main entry point for the application.
		/// </summary>
		[STAThread]
		static void Main() 
		{
			Application.Run(new PetsForm());
		}

		private void GetPetButton_Click(object sender, System.EventArgs e)
		{
			LogicServer ls;
			long term;

            term = 0;
			try
			{
				ls = new LogicServer();
				ls.Init("");
				ls.Load("pets.xpl");
				ls.AssertaStr("sound(" + SoundText.Text + ")");
				term = ls.ExecStr("pet(X)");
				if (term != 0)
					PetText.Text = ls.GetStrArg(term, 1);
				else
					PetText.Text = "Unknown Pet";
				ls.Close();
			}
			catch (LSException ex)
			{
				String message = ex.GetMessage();
                PetText.Text = message;
			}
		}


	}
}
