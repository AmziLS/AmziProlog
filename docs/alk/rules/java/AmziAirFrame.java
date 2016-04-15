import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import java.text.*;
//import com.borland.jbcl.layout.*;
import amzi.ls.*;

public class AmziAirFrame extends JFrame {
  JPanel contentPane;
  JMenuBar jMenuBar1 = new JMenuBar();
  JMenu jMenuFile = new JMenu();
  JMenuItem jMenuFileExit = new JMenuItem();
  JMenu jMenuHelp = new JMenu();
  JMenuItem jMenuHelpAbout = new JMenuItem();
  BorderLayout borderLayout1 = new BorderLayout();
  //XYLayout xYLayout1 = new XYLayout();
  JPanel jPanel1 = new JPanel();
  JComboBox jComboBoxDay = new JComboBox();
  JTextArea jTextAreaOutput = new JTextArea();
  JComboBox jComboBoxMonth = new JComboBox();
  JButton jButtonOK = new JButton();
  JLabel jLabel4 = new JLabel();
  JComboBox jComboBoxFrom = new JComboBox();
  JComboBox jComboBoxTo = new JComboBox();
  JLabel jLabel3 = new JLabel();
  JLabel jLabel2 = new JLabel();
  JPanel jPanel3 = new JPanel();
  JLabel jLabel1 = new JLabel();
  JPanel jPanel2 = new JPanel();

  String[] places = {
    "Lebanon, OH",
    "Winder, GA",
    "St. Petersburg, FL",
    "Russellville, AL",
    "Soddy Daisy, TN",
    "Lebanon, OH"
    };
  String[] days = {"Monday", "Tuesday", "Wednesday",
    "Thursday", "Friday", "Saturday", "Sunday"};
  String[] months = {"January", "February", "March",
    "April", "May", "June", "July", "August",
    "September", "October", "November", "December"};

  String from;
  String to;
  String day;
  String month;

  LogicServer ls = new LogicServer();
  GridBagLayout gridBagLayout1 = new GridBagLayout();
  BorderLayout borderLayout2 = new BorderLayout();
  GridBagLayout gridBagLayout2 = new GridBagLayout();

  /**Construct the frame*/
  public AmziAirFrame() {
    enableEvents(AWTEvent.WINDOW_EVENT_MASK);
    try {
      jbInit();
      fillComboBoxes();
      ls.Init("");
      ls.Load("amzi_air");
    }
    catch(Exception e) {
      e.printStackTrace();
    }
  }
  /**Component initialization*/
  private void jbInit() throws Exception  {
    //setIconImage(Toolkit.getDefaultToolkit().createImage(AmziAirFrame.class.getResource("[Your Icon]")));
    contentPane = (JPanel) this.getContentPane();
    this.setSize(new Dimension(343, 374));
    this.setTitle("Amzi! Air");
    jMenuFile.setText("File");
    jMenuFileExit.setText("Exit");
    jMenuFileExit.addActionListener(new ActionListener()  {
      public void actionPerformed(ActionEvent e) {
        jMenuFileExit_actionPerformed(e);
      }
    });
    jMenuHelp.setText("Help");
    jMenuHelpAbout.setText("About");
    jMenuHelpAbout.addActionListener(new ActionListener()  {
      public void actionPerformed(ActionEvent e) {
        jMenuHelpAbout_actionPerformed(e);
      }
    });
    jPanel1.setLayout(gridBagLayout2);
    jPanel1.setBackground(Color.gray);
    jPanel1.setBorder(BorderFactory.createRaisedBevelBorder());
    jButtonOK.setText("OK");
    jButtonOK.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(ActionEvent e) {
        jButtonOK_actionPerformed(e);
      }
    });
    jLabel4.setText("Day:");
    jLabel3.setText("Month: ");
    jLabel2.setText("To:");
    jPanel3.setLayout(borderLayout2);
    jPanel3.setBorder(BorderFactory.createEtchedBorder());
    jPanel3.setMinimumSize(new Dimension(210, 30));
    jPanel3.setPreferredSize(new Dimension(210, 30));
    jLabel1.setText("From: ");
    jPanel2.setBorder(BorderFactory.createEtchedBorder());
    jPanel2.setLayout(gridBagLayout1);
    jComboBoxFrom.setBackground(Color.white);
    jComboBoxFrom.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(ActionEvent e) {
        jComboBoxFrom_actionPerformed(e);
      }
    });
    jComboBoxDay.setBackground(Color.white);
    jComboBoxDay.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(ActionEvent e) {
        jComboBoxDay_actionPerformed(e);
      }
    });
    jComboBoxTo.setBackground(Color.white);
    jComboBoxTo.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(ActionEvent e) {
        jComboBoxTo_actionPerformed(e);
      }
    });
    jComboBoxMonth.setBackground(Color.white);
    jComboBoxMonth.addActionListener(new java.awt.event.ActionListener() {
      public void actionPerformed(ActionEvent e) {
        jComboBoxMonth_actionPerformed(e);
      }
    });
    contentPane.setMinimumSize(new Dimension(508, 508));
    jTextAreaOutput.setPreferredSize(new Dimension(200, 20));
    jTextAreaOutput.setMinimumSize(new Dimension(200, 20));
    jMenuFile.add(jMenuFileExit);
    jMenuHelp.add(jMenuHelpAbout);
    jMenuBar1.add(jMenuFile);
    jMenuBar1.add(jMenuHelp);
    this.setJMenuBar(jMenuBar1);
    contentPane.add(jPanel1, BorderLayout.CENTER);
    jPanel1.add(jPanel2, new GridBagConstraints(0, 0, 1, 1, 1.0, 1.0
            ,GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(5, 5, 5, 5), -4, 24));
    jPanel2.add(jLabel2, new GridBagConstraints(0, 0, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 5, 0, 0), 27, 0));
    jPanel2.add(jLabel1, new GridBagConstraints(0, 1, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 5, 0, 0), 3, 0));
    jPanel2.add(jComboBoxFrom, new GridBagConstraints(1, 1, 1, 1, 1.0, 0.0
            ,GridBagConstraints.CENTER, GridBagConstraints.HORIZONTAL, new Insets(5, 5, 5, 5), 134, 0));
    jPanel2.add(jLabel3, new GridBagConstraints(0, 2, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 5, 0, 0), 9, 0));
    jPanel2.add(jComboBoxMonth, new GridBagConstraints(1, 2, 1, 1, 1.0, 0.0
            ,GridBagConstraints.CENTER, GridBagConstraints.HORIZONTAL, new Insets(5, 5, 5, 5), 135, 0));
    jPanel2.add(jLabel4, new GridBagConstraints(0, 3, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(0, 5, 0, 0), 20, 0));
    jPanel2.add(jComboBoxDay, new GridBagConstraints(1, 3, 1, 1, 1.0, 0.0
            ,GridBagConstraints.CENTER, GridBagConstraints.HORIZONTAL, new Insets(5, 5, 5, 5), 127, 0));
    jPanel2.add(jButtonOK, new GridBagConstraints(1, 4, 1, 1, 0.0, 0.0
            ,GridBagConstraints.WEST, GridBagConstraints.NONE, new Insets(5, 5, 5, 5), 72, 0));
    jPanel2.add(jComboBoxTo, new GridBagConstraints(1, 0, 1, 1, 1.0, 0.0
            ,GridBagConstraints.CENTER, GridBagConstraints.HORIZONTAL, new Insets(5, 5, 5, 5), 135, 0));
    jPanel1.add(jPanel3, new GridBagConstraints(0, 1, 1, 1, 1.0, 1.0
            ,GridBagConstraints.CENTER, GridBagConstraints.BOTH, new Insets(5, 5, 5, 5), -64, 133));
    jPanel3.add(jTextAreaOutput, BorderLayout.CENTER);
  }
  /**File | Exit action performed*/
  public void jMenuFileExit_actionPerformed(ActionEvent e) {
    try { ls.Close(); } catch(LSException lsex) {};
    System.exit(0);
  }
  /**Help | About action performed*/
  public void jMenuHelpAbout_actionPerformed(ActionEvent e) {
  }
  /**Overridden so we can exit when window is closed*/
  protected void processWindowEvent(WindowEvent e) {
    super.processWindowEvent(e);
    if (e.getID() == WindowEvent.WINDOW_CLOSING) {
      jMenuFileExit_actionPerformed(null);
    }
  }

  void jComboBoxFrom_actionPerformed(ActionEvent e) {
    from = (String)jComboBoxFrom.getSelectedItem();
  }

  void jComboBoxTo_actionPerformed(ActionEvent e) {
    to = (String)jComboBoxTo.getSelectedItem();
  }

  void jComboBoxMonth_actionPerformed(ActionEvent e) {
    month = (String)jComboBoxMonth.getSelectedItem();
  }

  void jComboBoxDay_actionPerformed(ActionEvent e) {
    day = (String)jComboBoxDay.getSelectedItem();
  }

  void jButtonOK_actionPerformed(ActionEvent e) {
    jTextAreaOutput.setText("");
    jTextAreaOutput.append("From: " + from + "\n");
    jTextAreaOutput.append("To: " + to + "\n");
    jTextAreaOutput.append("Month: " + month + "\n");
    jTextAreaOutput.append("Day: " + day + "\n");
    long term;
    DecimalFormat myFormatter = new DecimalFormat("$###.00");

    try {
      ls.ExecStr("abolish(fact/2)");
      ls.AssertaStr("fact(month,'" + month + "')");
      ls.AssertaStr("fact(day,'" + day + "')");
      term = ls.ExecStr("price('" +from+ "','" +to+ "',PRICE)");
      if (term != 0) {
        double price = ls.GetFloatArg(term, 3);
        String price_string = myFormatter.format(price);
        jTextAreaOutput.append("\nPrice: " + price_string + "\n");
      }
      else
        jTextAreaOutput.append("no price available\n");
    }
    catch(LSException lsex) {
      jTextAreaOutput.append("*** Logic Server Exception ***\n");
      jTextAreaOutput.append(lsex.getMessage() + "\n");
    }
  }

  void fillComboBoxes() {
    int i;
    for (i=0; i < places.length; i++) {
      jComboBoxFrom.addItem(places[i]);
      jComboBoxTo.addItem(places[i]);
    }
    for (i=0; i < months.length; i++) {
      jComboBoxMonth.addItem(months[i]);
    }
    for (i=0; i < days.length; i++) {
      jComboBoxDay.addItem(days[i]);
    }
  }
}
