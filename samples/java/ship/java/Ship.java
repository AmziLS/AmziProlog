import java.awt.*;
import java.awt.event.*;
import amzi.ls.*;

public class Ship extends Frame
                  implements ActionListener
{
   public Ship()
   {
      Panel buttons = new Panel();
      buttons.setLayout(new FlowLayout());
      Button go = new Button("Go");
      go.addActionListener(this);
      buttons.add(go);
      Button exit = new Button("Exit");
      exit.addActionListener(this);
      buttons.add(exit);
      add("South", buttons);

      Panel inputs = new Panel();
      inputs.setLayout(new FlowLayout(FlowLayout.LEFT));

      pkgtype = new Choice();
      pkgtype.add("letter");
      pkgtype.add("brochure");
      pkgtype.add("package");
      inputs.add(pkgtype);

      inputs.add(new Label("Weight [0:1-4:15] lb:oz"));
      weight = new TextField("0:0", 5);
      inputs.add(weight);

      inputs.add(cod = new Checkbox("C.O.D."));
      inputs.add(new Label("U.P.S. Zone [2-8]"));

      zone = new TextField("2", 2);
      inputs.add(zone);

      inputs.add(new Label("Declared Value"));
      decvalue = new TextField("0", 5);
      inputs.add(decvalue);

      add("North", inputs);

      advice = new TextArea(8, 40);
      add("Center", advice);

      addWindowListener(new DWAdapter());
   }

   class DWAdapter extends WindowAdapter 
   {
      public void windowClosing(WindowEvent event) 
      {
         System.exit(0);
      }
   }

   public void actionPerformed(ActionEvent event)
   {
      Button source = (Button)event.getSource();
      if (source.getLabel().equals("Go"))
      {
         callExpert();
         repaint();
      }
      if (source.getLabel().equals("Exit"))
         System.exit(0);
   }

   public void callExpert()
   {
      long term;
      int amt;
      float famt;

      try
      {
      amzi.ls.LogicServer ls = new amzi.ls.LogicServer();

      ls.Init("");
      ls.Load("ship.xpl");

      /* Assert all the data collected from Java */
      ls.AssertaStr("known(weight, "+weight.getText()+")");
      if (cod.getState()) ls.AssertaStr("known(cod, yes)");
      else ls.AssertaStr("known(cod, no)");
      ls.AssertaStr("known(declared_value, "+decvalue.getText()+")");
      ls.AssertaStr("known(ups_zone, "+zone.getText()+")");
      ls.AssertaStr("known(type, "+pkgtype.getSelectedItem()+")");
      ls.AssertaStr("known(destination, 'USA')");

      term = ls.CallStr("option(Shipper, Service, Cost, Delivery)");
      if (term == 0)
         {
         advice.append("No shipping options available\n\n");
         ls.Close();
         return;
         }
      do
      {
         advice.append(ls.GetStrArg(term, 1));
         advice.append(" : ");
         advice.append(ls.GetStrArg(term, 2));
         advice.append("\n  $");
         amt = ls.GetIntArg(term, 3);
         famt = amt;
         advice.append(Float.toString(famt/100));
         
         advice.append(",  ");
         advice.append(ls.GetStrArg(term, 4));
         advice.append("\n\n");
      } while (ls.Redo());

      ls.Close();
      }
      catch (Throwable t)
      {
      t.printStackTrace();
      }

   }

   public static void displayStatus(String Msg)
   {
      System.out.println(Msg);
   }

   public static void main(String[] args)
   {
      Frame f = new Ship();
      f.setTitle("Shipping Advisor");
      f.show();
      f.pack();
   }

   private TextField weight;
   private TextField zone;
   private TextField decvalue;
   private Checkbox cod;
   private Choice pkgtype;
   private TextArea advice;
}

