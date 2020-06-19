/*
 * This file was automatically generated by EvoSuite
 * Tue Jun 09 11:30:09 GMT 2020
 */

package com.google.gson.stream;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import com.google.gson.stream.JsonWriter;
import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(mockJVMNonDeterminism = true, useVFS = true, useVNET = true, resetStaticState = true, separateClassLoader = false, useJEE = true) 
public class JsonWriter_ESTest extends JsonWriter_ESTest_scaffolding {

  @Test(timeout = 100000)
  public void test00()  throws Throwable  {
      Writer writer0 = mock(Writer.class, new ViolatedAssumptionAnswer());
      JsonWriter jsonWriter0 = new JsonWriter(writer0);
      JsonWriter jsonWriter1 = jsonWriter0.beginArray();
      JsonWriter jsonWriter2 = jsonWriter1.name("q6tJ/5-1]</yQ`B0/&");
      // Undeclared exception!
      try { 
        jsonWriter2.jsonValue("");
        fail("Expecting exception: IllegalStateException");
      
      } catch(IllegalStateException e) {
         //
         // Nesting problem.
         //
         verifyException("com.google.gson.stream.JsonWriter", e);
      }
  }

  @Test(timeout = 100000)
  public void test01()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      jsonWriter0.value("PfwdKRQo|y39%PF");
      assertEquals("\"PfwdKRQo|y39%PF\"", stringWriter0.toString());
  }

  @Test(timeout = 100000)
  public void test02()  throws Throwable  {
      Writer writer0 = mock(Writer.class, new ViolatedAssumptionAnswer());
      JsonWriter jsonWriter0 = new JsonWriter(writer0);
      JsonWriter jsonWriter1 = jsonWriter0.beginObject();
      JsonWriter jsonWriter2 = jsonWriter1.endObject();
      jsonWriter0.setLenient(true);
      jsonWriter2.setHtmlSafe(true);
      jsonWriter1.value(true);
      assertTrue(jsonWriter1.isHtmlSafe());
  }

  @Test(timeout = 100000)
  public void test03()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      jsonWriter0.setSerializeNulls(false);
      jsonWriter0.value(false);
      assertEquals("false", stringWriter0.toString());
  }

  @Test(timeout = 100000)
  public void test04()  throws Throwable  {
      Writer writer0 = mock(Writer.class, new ViolatedAssumptionAnswer());
      JsonWriter jsonWriter0 = new JsonWriter(writer0);
      jsonWriter0.setSerializeNulls(false);
      JsonWriter jsonWriter1 = jsonWriter0.value("");
      assertFalse(jsonWriter1.getSerializeNulls());
  }

  @Test(timeout = 100000)
  public void test05()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      jsonWriter0.setHtmlSafe(true);
      Integer integer0 = Integer.valueOf((-1744));
      jsonWriter0.value((Number) integer0);
      assertTrue(jsonWriter0.isHtmlSafe());
  }

  @Test(timeout = 100000)
  public void test06()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      jsonWriter0.setSerializeNulls(false);
      Integer integer0 = Integer.valueOf((-1744));
      JsonWriter jsonWriter1 = jsonWriter0.value((Number) integer0);
      assertFalse(jsonWriter1.getSerializeNulls());
  }

  @Test(timeout = 100000)
  public void test07()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter(0);
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      JsonWriter jsonWriter1 = jsonWriter0.beginObject();
      JsonWriter jsonWriter2 = jsonWriter0.name("+0");
      JsonWriter jsonWriter3 = jsonWriter1.value((-1.0));
      JsonWriter jsonWriter4 = jsonWriter1.name("");
      JsonWriter jsonWriter5 = jsonWriter3.beginArray();
      jsonWriter4.beginObject();
      jsonWriter4.name("+0");
      jsonWriter1.value((-1.0));
      jsonWriter3.name("");
      jsonWriter5.beginObject();
      jsonWriter3.endObject();
      jsonWriter1.endObject();
      jsonWriter2.setLenient(true);
      Boolean boolean0 = Boolean.valueOf("<J^Ow,)^'M9d`}6O(L");
      jsonWriter5.value(boolean0);
      assertEquals("{\"+0\":-1.0,\"\":[{\"+0\":-1.0,\"\":{}},false", stringWriter0.toString());
  }

  @Test(timeout = 100000)
  public void test08()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter(0);
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      JsonWriter jsonWriter1 = jsonWriter0.beginObject();
      jsonWriter0.name("+0");
      JsonWriter jsonWriter2 = jsonWriter1.value((-1.0));
      JsonWriter jsonWriter3 = jsonWriter1.name("");
      JsonWriter jsonWriter4 = jsonWriter2.beginArray();
      jsonWriter3.beginObject();
      jsonWriter3.name("+0");
      jsonWriter1.value((-1.0));
      jsonWriter2.name("");
      jsonWriter4.beginObject();
      jsonWriter2.endObject();
      JsonWriter jsonWriter5 = jsonWriter1.endObject();
      jsonWriter5.setHtmlSafe(true);
      Boolean boolean0 = Boolean.valueOf("<J^Ow,)^'M9d`}6O(L");
      jsonWriter4.value(boolean0);
      assertTrue(jsonWriter2.isHtmlSafe());
      assertEquals("{\"+0\":-1.0,\"\":[{\"+0\":-1.0,\"\":{}},false", stringWriter0.toString());
  }

  @Test(timeout = 100000)
  public void test09()  throws Throwable  {
      Writer writer0 = mock(Writer.class, new ViolatedAssumptionAnswer());
      JsonWriter jsonWriter0 = new JsonWriter(writer0);
      jsonWriter0.setSerializeNulls(false);
      JsonWriter jsonWriter1 = jsonWriter0.value((Boolean) null);
      assertFalse(jsonWriter1.isHtmlSafe());
  }

  @Test(timeout = 100000)
  public void test10()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      jsonWriter0.setLenient(true);
      jsonWriter0.value((long) (-1744));
      assertTrue(jsonWriter0.isLenient());
  }

  @Test(timeout = 100000)
  public void test11()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      jsonWriter0.setHtmlSafe(true);
      jsonWriter0.value((long) (-1744));
      assertTrue(jsonWriter0.isHtmlSafe());
  }

  @Test(timeout = 100000)
  public void test12()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      jsonWriter0.setSerializeNulls(false);
      jsonWriter0.value((long) (-1744));
      assertEquals("-1744", stringWriter0.toString());
  }

  @Test(timeout = 100000)
  public void test13()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      JsonWriter jsonWriter1 = jsonWriter0.beginArray();
      JsonWriter jsonWriter2 = jsonWriter1.beginObject();
      JsonWriter jsonWriter3 = jsonWriter0.name("");
      JsonWriter jsonWriter4 = jsonWriter3.value((-2201.48493438));
      jsonWriter4.name("java.lang.Integer@0000000003");
      assertEquals("[{\"\":-2201.48493438", stringWriter0.toString());
      
      jsonWriter2.beginObject();
      jsonWriter3.setHtmlSafe(true);
      jsonWriter2.endObject();
      JsonWriter jsonWriter5 = jsonWriter4.endObject();
      jsonWriter5.value((-2201.48493438));
      assertTrue(jsonWriter0.getSerializeNulls());
  }

  @Test(timeout = 100000)
  public void test14()  throws Throwable  {
      Writer writer0 = mock(Writer.class, new ViolatedAssumptionAnswer());
      doReturn((Writer) null).when(writer0).append(any(java.lang.CharSequence.class));
      JsonWriter jsonWriter0 = new JsonWriter(writer0);
      jsonWriter0.setSerializeNulls(false);
      JsonWriter jsonWriter1 = jsonWriter0.value(0.0);
      assertFalse(jsonWriter1.isHtmlSafe());
  }

  @Test(timeout = 100000)
  public void test15()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      jsonWriter0.setLenient(true);
      jsonWriter0.nullValue();
      assertEquals("null", stringWriter0.toString());
      assertTrue(jsonWriter0.isLenient());
  }

  @Test(timeout = 100000)
  public void test16()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      jsonWriter0.setHtmlSafe(true);
      jsonWriter0.nullValue();
      assertTrue(jsonWriter0.isHtmlSafe());
  }

  @Test(timeout = 100000)
  public void test17()  throws Throwable  {
      Writer writer0 = mock(Writer.class, new ViolatedAssumptionAnswer());
      JsonWriter jsonWriter0 = new JsonWriter(writer0);
      jsonWriter0.setLenient(true);
      jsonWriter0.name("java.lang.Float@0000000002");
      assertTrue(jsonWriter0.isLenient());
  }

  @Test(timeout = 100000)
  public void test18()  throws Throwable  {
      Writer writer0 = mock(Writer.class, new ViolatedAssumptionAnswer());
      JsonWriter jsonWriter0 = new JsonWriter(writer0);
      jsonWriter0.setHtmlSafe(true);
      jsonWriter0.name("");
      assertTrue(jsonWriter0.isHtmlSafe());
  }

  @Test(timeout = 100000)
  public void test19()  throws Throwable  {
      Writer writer0 = mock(Writer.class, new ViolatedAssumptionAnswer());
      JsonWriter jsonWriter0 = new JsonWriter(writer0);
      jsonWriter0.setSerializeNulls(false);
      JsonWriter jsonWriter1 = jsonWriter0.name("java.lang.Long@0000000002");
      assertFalse(jsonWriter1.getSerializeNulls());
  }

  @Test(timeout = 100000)
  public void test20()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      jsonWriter0.setLenient(true);
      jsonWriter0.jsonValue("kBs?Lm");
      assertEquals("kBs?Lm", stringWriter0.toString());
      assertTrue(jsonWriter0.isLenient());
  }

  @Test(timeout = 100000)
  public void test21()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      jsonWriter0.setHtmlSafe(true);
      jsonWriter0.jsonValue("kBs?Lm");
      assertEquals("kBs?Lm", stringWriter0.toString());
      assertTrue(jsonWriter0.getSerializeNulls());
  }

  @Test(timeout = 100000)
  public void test22()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      jsonWriter0.setSerializeNulls(false);
      jsonWriter0.jsonValue("kBs?Lm");
      assertEquals("kBs?Lm", stringWriter0.toString());
  }

  @Test(timeout = 100000)
  public void test23()  throws Throwable  {
      Writer writer0 = mock(Writer.class, new ViolatedAssumptionAnswer());
      JsonWriter jsonWriter0 = new JsonWriter(writer0);
      jsonWriter0.setLenient(true);
      boolean boolean0 = jsonWriter0.isLenient();
      assertTrue(boolean0);
  }

  @Test(timeout = 100000)
  public void test24()  throws Throwable  {
      Writer writer0 = mock(Writer.class, new ViolatedAssumptionAnswer());
      JsonWriter jsonWriter0 = new JsonWriter(writer0);
      jsonWriter0.setHtmlSafe(true);
      boolean boolean0 = jsonWriter0.isHtmlSafe();
      assertTrue(boolean0);
  }

  @Test(timeout = 100000)
  public void test25()  throws Throwable  {
      Writer writer0 = mock(Writer.class, new ViolatedAssumptionAnswer());
      JsonWriter jsonWriter0 = new JsonWriter(writer0);
      jsonWriter0.setSerializeNulls(false);
      boolean boolean0 = jsonWriter0.getSerializeNulls();
      assertFalse(boolean0);
  }

  @Test(timeout = 100000)
  public void test26()  throws Throwable  {
      Writer writer0 = mock(Writer.class, new ViolatedAssumptionAnswer());
      JsonWriter jsonWriter0 = new JsonWriter(writer0);
      jsonWriter0.setSerializeNulls(false);
      JsonWriter jsonWriter1 = jsonWriter0.beginObject();
      JsonWriter jsonWriter2 = jsonWriter1.endObject();
      assertSame(jsonWriter2, jsonWriter0);
  }

  @Test(timeout = 100000)
  public void test27()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter(0);
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      JsonWriter jsonWriter1 = jsonWriter0.beginObject();
      JsonWriter jsonWriter2 = jsonWriter0.name("+0");
      JsonWriter jsonWriter3 = jsonWriter1.value((-1.0));
      JsonWriter jsonWriter4 = jsonWriter1.name("");
      JsonWriter jsonWriter5 = jsonWriter3.beginArray();
      jsonWriter4.beginObject();
      jsonWriter4.name("+0");
      jsonWriter1.value((-1.0));
      jsonWriter3.name("");
      jsonWriter5.beginObject();
      jsonWriter3.endObject();
      jsonWriter1.endObject();
      jsonWriter2.setLenient(true);
      jsonWriter5.endArray();
      assertEquals("{\"+0\":-1.0,\"\":[{\"+0\":-1.0,\"\":{}}]", stringWriter0.toString());
  }

  @Test(timeout = 100000)
  public void test28()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      JsonWriter jsonWriter1 = jsonWriter0.beginArray();
      JsonWriter jsonWriter2 = jsonWriter1.beginObject();
      JsonWriter jsonWriter3 = jsonWriter0.name("");
      JsonWriter jsonWriter4 = jsonWriter3.value((-2201.48493438));
      JsonWriter jsonWriter5 = jsonWriter4.name("java.lang.Integer@0000000003");
      jsonWriter2.beginObject();
      jsonWriter3.setHtmlSafe(true);
      jsonWriter2.endObject();
      jsonWriter4.endObject();
      jsonWriter5.endArray();
      assertTrue(jsonWriter3.isHtmlSafe());
      assertTrue(jsonWriter0.isHtmlSafe());
  }

  @Test(timeout = 100000)
  public void test29()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      JsonWriter jsonWriter1 = jsonWriter0.beginObject();
      JsonWriter jsonWriter2 = jsonWriter0.name("S!ac>L?EL@.+");
      jsonWriter2.setSerializeNulls(false);
      jsonWriter1.beginArray();
      jsonWriter2.endArray();
      assertEquals("{\"S!ac>L?EL@.+\":[]", stringWriter0.toString());
  }

  @Test(timeout = 100000)
  public void test30()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      jsonWriter0.setLenient(true);
      jsonWriter0.beginObject();
      jsonWriter0.endObject();
      assertTrue(jsonWriter0.isLenient());
  }

  @Test(timeout = 100000)
  public void test31()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      jsonWriter0.setHtmlSafe(true);
      jsonWriter0.beginObject();
      assertTrue(jsonWriter0.isHtmlSafe());
  }

  @Test(timeout = 100000)
  public void test32()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      jsonWriter0.setLenient(true);
      jsonWriter0.beginArray();
      assertTrue(jsonWriter0.isLenient());
  }

  @Test(timeout = 100000)
  public void test33()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      jsonWriter0.setHtmlSafe(true);
      jsonWriter0.beginArray();
      assertTrue(jsonWriter0.isHtmlSafe());
  }

  @Test(timeout = 100000)
  public void test34()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      jsonWriter0.beginObject();
      // Undeclared exception!
      try { 
        jsonWriter0.value(true);
        fail("Expecting exception: IllegalStateException");
      
      } catch(IllegalStateException e) {
         //
         // Nesting problem.
         //
         verifyException("com.google.gson.stream.JsonWriter", e);
      }
  }

  @Test(timeout = 100000)
  public void test35()  throws Throwable  {
      Writer writer0 = mock(Writer.class, new ViolatedAssumptionAnswer());
      JsonWriter jsonWriter0 = new JsonWriter(writer0);
      jsonWriter0.nullValue();
      // Undeclared exception!
      try { 
        jsonWriter0.value((String) null);
        fail("Expecting exception: IllegalStateException");
      
      } catch(IllegalStateException e) {
         //
         // JSON must have only one top-level value.
         //
         verifyException("com.google.gson.stream.JsonWriter", e);
      }
  }

  @Test(timeout = 100000)
  public void test36()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      JsonWriter jsonWriter1 = jsonWriter0.nullValue();
      // Undeclared exception!
      try { 
        jsonWriter1.value((Number) null);
        fail("Expecting exception: IllegalStateException");
      
      } catch(IllegalStateException e) {
         //
         // JSON must have only one top-level value.
         //
         verifyException("com.google.gson.stream.JsonWriter", e);
      }
  }

  @Test(timeout = 100000)
  public void test37()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      JsonWriter jsonWriter1 = jsonWriter0.nullValue();
      Boolean boolean0 = Boolean.TRUE;
      // Undeclared exception!
      try { 
        jsonWriter1.value(boolean0);
        fail("Expecting exception: IllegalStateException");
      
      } catch(IllegalStateException e) {
         //
         // JSON must have only one top-level value.
         //
         verifyException("com.google.gson.stream.JsonWriter", e);
      }
  }

  @Test(timeout = 100000)
  public void test38()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      JsonWriter jsonWriter1 = jsonWriter0.beginObject();
      // Undeclared exception!
      try { 
        jsonWriter1.value(0L);
        fail("Expecting exception: IllegalStateException");
      
      } catch(IllegalStateException e) {
         //
         // Nesting problem.
         //
         verifyException("com.google.gson.stream.JsonWriter", e);
      }
  }

  @Test(timeout = 100000)
  public void test39()  throws Throwable  {
      Writer writer0 = mock(Writer.class, new ViolatedAssumptionAnswer());
      JsonWriter jsonWriter0 = new JsonWriter(writer0);
      jsonWriter0.beginObject();
      // Undeclared exception!
      try { 
        jsonWriter0.value((-2679.2505));
        fail("Expecting exception: IllegalStateException");
      
      } catch(IllegalStateException e) {
         //
         // Nesting problem.
         //
         verifyException("com.google.gson.stream.JsonWriter", e);
      }
  }

  @Test(timeout = 100000)
  public void test40()  throws Throwable  {
      Writer writer0 = mock(Writer.class, new ViolatedAssumptionAnswer());
      JsonWriter jsonWriter0 = new JsonWriter(writer0);
      // Undeclared exception!
      try { 
        jsonWriter0.setIndent((String) null);
        fail("Expecting exception: NullPointerException");
      
      } catch(NullPointerException e) {
         //
         // no message in exception (getMessage() returned null)
         //
         verifyException("com.google.gson.stream.JsonWriter", e);
      }
  }

  @Test(timeout = 100000)
  public void test41()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      JsonWriter jsonWriter1 = jsonWriter0.value("as18-*Ju!Z]pNIw';u");
      // Undeclared exception!
      try { 
        jsonWriter1.jsonValue("as18-*Ju!Z]pNIw';u");
        fail("Expecting exception: IllegalStateException");
      
      } catch(IllegalStateException e) {
         //
         // JSON must have only one top-level value.
         //
         verifyException("com.google.gson.stream.JsonWriter", e);
      }
  }

  @Test(timeout = 100000)
  public void test42()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      jsonWriter0.beginObject();
      // Undeclared exception!
      try { 
        jsonWriter0.beginArray();
        fail("Expecting exception: IllegalStateException");
      
      } catch(IllegalStateException e) {
         //
         // Nesting problem.
         //
         verifyException("com.google.gson.stream.JsonWriter", e);
      }
  }

  @Test(timeout = 100000)
  public void test43()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      JsonWriter jsonWriter1 = jsonWriter0.beginObject();
      JsonWriter jsonWriter2 = jsonWriter1.name("\r");
      jsonWriter2.beginObject();
      assertEquals("{\"\\r\":{", stringWriter0.toString());
  }

  @Test(timeout = 100000)
  public void test44()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      JsonWriter jsonWriter1 = jsonWriter0.beginArray();
      JsonWriter jsonWriter2 = jsonWriter1.beginArray();
      JsonWriter jsonWriter3 = jsonWriter1.beginObject();
      JsonWriter jsonWriter4 = jsonWriter2.name("");
      jsonWriter4.beginArray();
      JsonWriter jsonWriter5 = jsonWriter2.beginArray();
      JsonWriter jsonWriter6 = jsonWriter0.beginArray();
      JsonWriter jsonWriter7 = jsonWriter2.beginArray();
      JsonWriter jsonWriter8 = jsonWriter6.beginArray();
      JsonWriter jsonWriter9 = jsonWriter7.beginArray();
      JsonWriter jsonWriter10 = jsonWriter6.beginArray();
      JsonWriter jsonWriter11 = jsonWriter3.beginArray();
      JsonWriter jsonWriter12 = jsonWriter6.beginArray();
      jsonWriter4.beginArray();
      jsonWriter10.beginObject();
      JsonWriter jsonWriter13 = jsonWriter8.name("");
      JsonWriter jsonWriter14 = jsonWriter13.beginArray();
      jsonWriter14.beginArray();
      jsonWriter9.beginArray();
      jsonWriter11.beginArray();
      jsonWriter1.beginArray();
      JsonWriter jsonWriter15 = jsonWriter5.beginArray();
      jsonWriter1.beginArray();
      JsonWriter jsonWriter16 = jsonWriter12.beginArray();
      jsonWriter16.beginArray();
      jsonWriter13.beginArray();
      jsonWriter15.beginArray();
      JsonWriter jsonWriter17 = jsonWriter16.beginArray();
      jsonWriter17.beginArray();
      jsonWriter17.beginArray();
      jsonWriter5.beginArray();
      JsonWriter jsonWriter18 = jsonWriter3.beginArray();
      jsonWriter18.beginArray();
      jsonWriter13.beginArray();
      assertEquals("[[{\"\":[[[[[[[[[[{\"\":[[[[[[[[[[[[[[[[[[", stringWriter0.toString());
  }

  @Test(timeout = 100000)
  public void test45()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      JsonWriter jsonWriter1 = jsonWriter0.nullValue();
      jsonWriter1.setLenient(true);
      jsonWriter1.value("9[a\"SG5z)Lfz7Bcl");
      assertEquals("null\"9[a\\\"SG5z)Lfz7Bcl\"", stringWriter0.toString());
  }

  @Test(timeout = 100000)
  public void test46()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      JsonWriter jsonWriter1 = jsonWriter0.nullValue();
      // Undeclared exception!
      try { 
        jsonWriter1.beginObject();
        fail("Expecting exception: IllegalStateException");
      
      } catch(IllegalStateException e) {
         //
         // JSON must have only one top-level value.
         //
         verifyException("com.google.gson.stream.JsonWriter", e);
      }
  }

  @Test(timeout = 100000)
  public void test47()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      JsonWriter jsonWriter1 = jsonWriter0.beginObject();
      jsonWriter1.name("\r");
      JsonWriter jsonWriter2 = jsonWriter0.value(570.11366014);
      // Undeclared exception!
      try { 
        jsonWriter2.beginObject();
        fail("Expecting exception: IllegalStateException");
      
      } catch(IllegalStateException e) {
         //
         // Nesting problem.
         //
         verifyException("com.google.gson.stream.JsonWriter", e);
      }
  }

  @Test(timeout = 100000)
  public void test48()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      JsonWriter jsonWriter1 = jsonWriter0.beginObject();
      // Undeclared exception!
      try { 
        jsonWriter1.beginObject();
        fail("Expecting exception: IllegalStateException");
      
      } catch(IllegalStateException e) {
         //
         // Nesting problem.
         //
         verifyException("com.google.gson.stream.JsonWriter", e);
      }
  }

  @Test(timeout = 100000)
  public void test49()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      JsonWriter jsonWriter1 = jsonWriter0.beginArray();
      jsonWriter1.nullValue();
      jsonWriter0.beginArray();
      assertEquals("[null,[", stringWriter0.toString());
  }

  @Test(timeout = 100000)
  public void test50()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      JsonWriter jsonWriter1 = jsonWriter0.beginObject();
      jsonWriter1.name("\r");
      JsonWriter jsonWriter2 = jsonWriter0.nullValue();
      jsonWriter2.name("\r");
      jsonWriter1.value((Number) null);
      assertEquals("{\"\\r\":null,\"\\r\":null", stringWriter0.toString());
  }

  @Test(timeout = 100000)
  public void test51()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      JsonWriter jsonWriter1 = jsonWriter0.beginArray();
      jsonWriter0.setIndent("#7[A_q'a@0[Cs2FJ[o");
      assertEquals("[", stringWriter0.toString());
      
      jsonWriter1.beginObject();
      assertTrue(jsonWriter0.getSerializeNulls());
  }

  @Test(timeout = 100000)
  public void test52()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      try { 
        jsonWriter0.close();
        fail("Expecting exception: IOException");
      
      } catch(IOException e) {
         //
         // Incomplete document
         //
         verifyException("com.google.gson.stream.JsonWriter", e);
      }
  }

  @Test(timeout = 100000)
  public void test53()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      JsonWriter jsonWriter1 = jsonWriter0.nullValue();
      jsonWriter1.close();
      jsonWriter0.close();
      assertEquals("null", stringWriter0.toString());
  }

  @Test(timeout = 100000)
  public void test54()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      JsonWriter jsonWriter1 = jsonWriter0.beginObject();
      try { 
        jsonWriter1.close();
        fail("Expecting exception: IOException");
      
      } catch(IOException e) {
         //
         // Incomplete document
         //
         verifyException("com.google.gson.stream.JsonWriter", e);
      }
  }

  @Test(timeout = 100000)
  public void test55()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      JsonWriter jsonWriter1 = jsonWriter0.nullValue();
      jsonWriter1.close();
      // Undeclared exception!
      try { 
        jsonWriter0.flush();
        fail("Expecting exception: IllegalStateException");
      
      } catch(IllegalStateException e) {
         //
         // JsonWriter is closed.
         //
         verifyException("com.google.gson.stream.JsonWriter", e);
      }
  }

  @Test(timeout = 100000)
  public void test56()  throws Throwable  {
      Writer writer0 = mock(Writer.class, new ViolatedAssumptionAnswer());
      JsonWriter jsonWriter0 = new JsonWriter(writer0);
      jsonWriter0.flush();
      assertTrue(jsonWriter0.getSerializeNulls());
  }

  @Test(timeout = 100000)
  public void test57()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      jsonWriter0.setLenient(true);
      Byte byte0 = new Byte((byte)20);
      jsonWriter0.value((Number) byte0);
      assertTrue(jsonWriter0.isLenient());
  }

  @Test(timeout = 100000)
  public void test58()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      jsonWriter0.setLenient(true);
      jsonWriter0.value(0.0);
      assertEquals("0.0", stringWriter0.toString());
      assertTrue(jsonWriter0.getSerializeNulls());
  }

  @Test(timeout = 100000)
  public void test59()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      Boolean boolean0 = Boolean.TRUE;
      jsonWriter0.value(boolean0);
      assertEquals("true", stringWriter0.toString());
  }

  @Test(timeout = 100000)
  public void test60()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      JsonWriter jsonWriter1 = jsonWriter0.name("H;L`X^Nd;");
      assertTrue(jsonWriter1.getSerializeNulls());
      
      jsonWriter0.setSerializeNulls(false);
      assertFalse(jsonWriter0.getSerializeNulls());
      
      jsonWriter0.nullValue();
      assertEquals("", stringWriter0.toString());
  }

  @Test(timeout = 100000)
  public void test61()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      jsonWriter0.name("\r");
      // Undeclared exception!
      try { 
        jsonWriter0.nullValue();
        fail("Expecting exception: IllegalStateException");
      
      } catch(IllegalStateException e) {
         //
         // Nesting problem.
         //
         verifyException("com.google.gson.stream.JsonWriter", e);
      }
  }

  @Test(timeout = 100000)
  public void test62()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      jsonWriter0.jsonValue((String) null);
      assertEquals("null", stringWriter0.toString());
  }

  @Test(timeout = 100000)
  public void test63()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      JsonWriter jsonWriter1 = jsonWriter0.nullValue();
      jsonWriter1.close();
      // Undeclared exception!
      try { 
        jsonWriter0.name("java.lang.Float@0000000003");
        fail("Expecting exception: IllegalStateException");
      
      } catch(IllegalStateException e) {
         //
         // JsonWriter is closed.
         //
         verifyException("com.google.gson.stream.JsonWriter", e);
      }
  }

  @Test(timeout = 100000)
  public void test64()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      jsonWriter0.name("");
      // Undeclared exception!
      try { 
        jsonWriter0.name("");
        fail("Expecting exception: IllegalStateException");
      
      } catch(IllegalStateException e) {
         //
         // no message in exception (getMessage() returned null)
         //
         verifyException("com.google.gson.stream.JsonWriter", e);
      }
  }

  @Test(timeout = 100000)
  public void test65()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      // Undeclared exception!
      try { 
        jsonWriter0.name((String) null);
        fail("Expecting exception: NullPointerException");
      
      } catch(NullPointerException e) {
         //
         // name == null
         //
         verifyException("com.google.gson.stream.JsonWriter", e);
      }
  }

  @Test(timeout = 100000)
  public void test66()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      JsonWriter jsonWriter1 = jsonWriter0.nullValue();
      jsonWriter1.close();
      // Undeclared exception!
      try { 
        jsonWriter0.beginObject();
        fail("Expecting exception: IllegalStateException");
      
      } catch(IllegalStateException e) {
         //
         // JsonWriter is closed.
         //
         verifyException("com.google.gson.stream.JsonWriter", e);
      }
  }

  @Test(timeout = 100000)
  public void test67()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      JsonWriter jsonWriter1 = jsonWriter0.beginArray();
      JsonWriter jsonWriter2 = jsonWriter1.name("q:`TJ");
      // Undeclared exception!
      try { 
        jsonWriter2.endArray();
        fail("Expecting exception: IllegalStateException");
      
      } catch(IllegalStateException e) {
         //
         // Dangling name: q:`TJ
         //
         verifyException("com.google.gson.stream.JsonWriter", e);
      }
  }

  @Test(timeout = 100000)
  public void test68()  throws Throwable  {
      Writer writer0 = mock(Writer.class, new ViolatedAssumptionAnswer());
      JsonWriter jsonWriter0 = new JsonWriter(writer0);
      jsonWriter0.setIndent("");
      assertTrue(jsonWriter0.getSerializeNulls());
  }

  @Test(timeout = 100000)
  public void test69()  throws Throwable  {
      JsonWriter jsonWriter0 = null;
      try {
        jsonWriter0 = new JsonWriter((Writer) null);
        fail("Expecting exception: NullPointerException");
      
      } catch(NullPointerException e) {
         //
         // out == null
         //
         verifyException("com.google.gson.stream.JsonWriter", e);
      }
  }

  @Test(timeout = 100000)
  public void test70()  throws Throwable  {
      Writer writer0 = mock(Writer.class, new ViolatedAssumptionAnswer());
      JsonWriter jsonWriter0 = new JsonWriter(writer0);
      JsonWriter jsonWriter1 = jsonWriter0.beginArray();
      // Undeclared exception!
      try { 
        jsonWriter1.endObject();
        fail("Expecting exception: IllegalStateException");
      
      } catch(IllegalStateException e) {
         //
         // Nesting problem.
         //
         verifyException("com.google.gson.stream.JsonWriter", e);
      }
  }

  @Test(timeout = 100000)
  public void test71()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      // Undeclared exception!
      try { 
        jsonWriter0.endObject();
        fail("Expecting exception: IllegalStateException");
      
      } catch(IllegalStateException e) {
         //
         // Nesting problem.
         //
         verifyException("com.google.gson.stream.JsonWriter", e);
      }
  }

  @Test(timeout = 100000)
  public void test72()  throws Throwable  {
      Writer writer0 = mock(Writer.class, new ViolatedAssumptionAnswer());
      JsonWriter jsonWriter0 = new JsonWriter(writer0);
      boolean boolean0 = jsonWriter0.getSerializeNulls();
      assertTrue(boolean0);
  }

  @Test(timeout = 100000)
  public void test73()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      jsonWriter0.isLenient();
      assertTrue(jsonWriter0.getSerializeNulls());
  }

  @Test(timeout = 100000)
  public void test74()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      jsonWriter0.isHtmlSafe();
      assertTrue(jsonWriter0.getSerializeNulls());
  }

  @Test(timeout = 100000)
  public void test75()  throws Throwable  {
      StringWriter stringWriter0 = new StringWriter();
      JsonWriter jsonWriter0 = new JsonWriter(stringWriter0);
      jsonWriter0.setHtmlSafe(true);
      jsonWriter0.value("<");
      assertEquals("\"\\u003c\"", stringWriter0.toString());
  }
}