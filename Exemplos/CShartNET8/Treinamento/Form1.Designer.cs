namespace Treinamento
{
    partial class Form1
    {
        /// <summary>
        ///  Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        ///  Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        ///  Required method for Designer support - do not modify
        ///  the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            btnMDFeSincrono = new Button();
            SuspendLayout();
            // 
            // btnMDFeSincrono
            // 
            btnMDFeSincrono.Location = new Point(17, 29);
            btnMDFeSincrono.Name = "btnMDFeSincrono";
            btnMDFeSincrono.Size = new Size(192, 34);
            btnMDFeSincrono.TabIndex = 0;
            btnMDFeSincrono.Text = "MDFe Síncrono";
            btnMDFeSincrono.UseVisualStyleBackColor = true;
            btnMDFeSincrono.Click += btnMDFeSincrono_Click;
            // 
            // Form1
            // 
            AutoScaleDimensions = new SizeF(10F, 25F);
            AutoScaleMode = AutoScaleMode.Font;
            ClientSize = new Size(800, 450);
            Controls.Add(btnMDFeSincrono);
            Name = "Form1";
            Text = "Form1";
            ResumeLayout(false);
        }

        #endregion

        private Button btnMDFeSincrono;
    }
}
