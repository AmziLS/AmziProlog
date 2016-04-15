package ship;

import amzi.ls.*;

import java.text.NumberFormat;

/**
 * <p>Title: JSP Ship</p>
 * <p>Description: An implementation of the shipping advisor for JSP</p>
 * <p>Copyright: Copyright (c) 2004 Amzi! inc. All Rights Reserved.</p>
 * <p>Company: Amzi! inc.</p>
 */

public class shipPageBean {
   private String upsZone;
   private String type;
   private String weight;
   private String cod;
   private String declaredValue;

   // Set all our inputs
   public String getUpsZone() {
      return upsZone;
   }
   public void setUpsZone(String newValue) {
      if (newValue!=null) {
         upsZone = newValue;
      }
   }
   public String getType() {
      return type;
   }
   public void setType(String newValue) {
      if (newValue!=null) {
         type = newValue;
      }
   }
   public String getWeight() {
      return weight;
   }
   public void setWeight(String newValue) {
      if (newValue!=null) {
         weight = newValue;
      }
   }
   public String getCod() {
      return cod;
   }
   public void setCod(String newValue) {
      if (newValue!=null) {
         cod = newValue;
      }
   }
   public String getDeclaredValue() {
      return declaredValue;
   }
   public void setDeclaredValue(String newValue) {
      if (newValue!=null) {
         declaredValue = newValue;
      }
   }

   // Determine the shipping options
   public String getOptions() {
      long term;
      int amt;
      double damt;
      String options;

      amzi.ls.LogicServer ls = new amzi.ls.LogicServer();

      try
      {
         ls.Init("");
         ls.Load("C:/amzi/dev/servlets/ship/defaultroot/WEB-INF/ship.xpl");

         /* Assert all the data collected from Java */
         ls.AssertaStr("known(weight, "+weight+")");
         ls.AssertaStr("known(cod, "+cod+")");
         ls.AssertaStr("known(declared_value, "+declaredValue+")");
         ls.AssertaStr("known(ups_zone, "+upsZone+")");
         ls.AssertaStr("known(type, "+type+")");
         ls.AssertaStr("known(destination, 'USA')");

         term = ls.CallStr("option(Shipper, Service, Cost, Delivery)");
         if (term == 0) {
            options = "No shipping options found";
            ls.Close();
            return options;
         }
         options = "<table width=\"100%\" border=\"1\" cellpadding=\"2\" cellspacing=\"3\"><tr>"+
            "<td><font color=\"blue\" size=\"+2\">Shipper</font></td>"+
            "<td><font color=\"blue\" size=\"+2\">Service</font></td>"+
            "<td><font color=\"blue\" size=\"+2\">Cost</font></td>"+
            "<td><font color=\"blue\" size=\"+2\">Delivery</font></td></tr>";
         do {
            options += "<tr>";
            options += "<td>"+ls.GetStrArg(term, 1)+"</td>";
            options += "<td>"+ls.GetStrArg(term, 2)+"</td>";
            amt = ls.GetIntArg(term, 3);
            damt = amt/100;
            options += "<td>"+NumberFormat.getCurrencyInstance().format(damt)+"</td>";
            options += "<td>"+ls.GetStrArg(term, 4)+"</td>";
            options += "</tr>";
         } while (ls.Redo());
         options += "</table>";

         ls.Close();
      }
      catch (LSException ex) {
         try { ls.Close(); } catch (LSException ex2) {};
         options = "ERROR: " + ex.getMessage();
      }

      return options;
   }
   public void setOptions(String newValue) {
   }

}