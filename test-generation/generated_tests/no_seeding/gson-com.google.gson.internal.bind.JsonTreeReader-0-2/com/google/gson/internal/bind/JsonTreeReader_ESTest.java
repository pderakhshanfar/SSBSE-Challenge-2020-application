/*
 * This file was automatically generated by EvoSuite
 * Wed Jun 03 15:19:01 GMT 2020
 */

package com.google.gson.internal.bind;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonNull;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import com.google.gson.internal.bind.JsonTreeReader;
import com.google.gson.stream.JsonToken;
import java.util.ConcurrentModificationException;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(mockJVMNonDeterminism = true, useVFS = true, useVNET = true, resetStaticState = true, separateClassLoader = false, useJEE = true) 
public class JsonTreeReader_ESTest extends JsonTreeReader_ESTest_scaffolding {

  @Test(timeout = 100000)
  public void test00()  throws Throwable  {
      JsonArray jsonArray0 = new JsonArray();
      jsonArray0.add("FB]W|S6WGN.");
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonArray0);
      jsonTreeReader0.beginArray();
      // Undeclared exception!
      try { 
        jsonTreeReader0.beginArray();
        fail("Expecting exception: IllegalStateException");
      
      } catch(IllegalStateException e) {
         //
         // Expected BEGIN_ARRAY but was STRING at path $[0]
         //
         verifyException("com.google.gson.internal.bind.JsonTreeReader", e);
      }
  }

  @Test(timeout = 100000)
  public void test01()  throws Throwable  {
      JsonObject jsonObject0 = new JsonObject();
      Byte byte0 = new Byte((byte)0);
      jsonObject0.addProperty(")@)<q>r 4@|R", (Number) byte0);
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonObject0);
      jsonTreeReader0.beginObject();
      String string0 = jsonTreeReader0.nextName();
      assertEquals(")@)<q>r 4@|R", string0);
  }

  @Test(timeout = 100000)
  public void test02()  throws Throwable  {
      JsonObject jsonObject0 = new JsonObject();
      Boolean boolean0 = new Boolean("");
      jsonObject0.addProperty("", boolean0);
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonObject0);
      jsonTreeReader0.beginObject();
      String string0 = jsonTreeReader0.nextName();
      assertEquals("", string0);
  }

  @Test(timeout = 100000)
  public void test03()  throws Throwable  {
      Short short0 = new Short((short) (-5371));
      JsonPrimitive jsonPrimitive0 = new JsonPrimitive(short0);
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonPrimitive0);
      long long0 = jsonTreeReader0.nextLong();
      assertEquals((-5371L), long0);
  }

  @Test(timeout = 100000)
  public void test04()  throws Throwable  {
      JsonObject jsonObject0 = new JsonObject();
      Byte byte0 = Byte.valueOf((byte)0);
      jsonObject0.addProperty(")@)<q>r 4@|R", (Number) byte0);
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonObject0);
      jsonTreeReader0.beginObject();
      jsonTreeReader0.skipValue();
      int int0 = jsonTreeReader0.nextInt();
      assertEquals(0, int0);
  }

  @Test(timeout = 100000)
  public void test05()  throws Throwable  {
      Byte byte0 = new Byte((byte) (-34));
      JsonPrimitive jsonPrimitive0 = new JsonPrimitive(byte0);
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonPrimitive0);
      int int0 = jsonTreeReader0.nextInt();
      assertEquals((-34), int0);
  }

  @Test(timeout = 100000)
  public void test06()  throws Throwable  {
      JsonObject jsonObject0 = new JsonObject();
      Byte byte0 = new Byte((byte) (-21));
      jsonObject0.addProperty(")@)<q>r 4@|R", (Number) byte0);
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonObject0);
      jsonTreeReader0.beginObject();
      jsonTreeReader0.skipValue();
      double double0 = jsonTreeReader0.nextDouble();
      assertEquals((-21.0), double0, 0.01);
  }

  @Test(timeout = 100000)
  public void test07()  throws Throwable  {
      Boolean boolean0 = new Boolean(" at path ");
      JsonPrimitive jsonPrimitive0 = new JsonPrimitive(boolean0);
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonPrimitive0);
      boolean boolean1 = jsonTreeReader0.nextBoolean();
      assertFalse(boolean1);
  }

  @Test(timeout = 100000)
  public void test08()  throws Throwable  {
      JsonObject jsonObject0 = new JsonObject();
      Boolean boolean0 = Boolean.FALSE;
      jsonObject0.addProperty("", boolean0);
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonObject0);
      jsonTreeReader0.beginObject();
      jsonObject0.remove("");
      // Undeclared exception!
      try { 
        jsonTreeReader0.skipValue();
        fail("Expecting exception: ConcurrentModificationException");
      
      } catch(ConcurrentModificationException e) {
         //
         // no message in exception (getMessage() returned null)
         //
         verifyException("com.google.gson.internal.LinkedTreeMap$LinkedTreeMapIterator", e);
      }
  }

  @Test(timeout = 100000)
  public void test09()  throws Throwable  {
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader((JsonElement) null);
      // Undeclared exception!
      try { 
        jsonTreeReader0.skipValue();
        fail("Expecting exception: AssertionError");
      
      } catch(AssertionError e) {
         //
         // no message in exception (getMessage() returned null)
         //
      }
  }

  @Test(timeout = 100000)
  public void test10()  throws Throwable  {
      JsonObject jsonObject0 = new JsonObject();
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonObject0);
      jsonTreeReader0.skipValue();
      // Undeclared exception!
      try { 
        jsonTreeReader0.skipValue();
        fail("Expecting exception: ArrayIndexOutOfBoundsException");
      
      } catch(ArrayIndexOutOfBoundsException e) {
         //
         // -1
         //
         verifyException("com.google.gson.internal.bind.JsonTreeReader", e);
      }
  }

  @Test(timeout = 100000)
  public void test11()  throws Throwable  {
      JsonObject jsonObject0 = new JsonObject();
      Boolean boolean0 = Boolean.FALSE;
      jsonObject0.addProperty("Expected null but was ", boolean0);
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonObject0);
      jsonTreeReader0.beginObject();
      jsonObject0.remove("Expected null but was ");
      // Undeclared exception!
      try { 
        jsonTreeReader0.promoteNameToValue();
        fail("Expecting exception: ConcurrentModificationException");
      
      } catch(ConcurrentModificationException e) {
         //
         // no message in exception (getMessage() returned null)
         //
         verifyException("com.google.gson.internal.LinkedTreeMap$LinkedTreeMapIterator", e);
      }
  }

  @Test(timeout = 100000)
  public void test12()  throws Throwable  {
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader((JsonElement) null);
      // Undeclared exception!
      try { 
        jsonTreeReader0.promoteNameToValue();
        fail("Expecting exception: AssertionError");
      
      } catch(AssertionError e) {
         //
         // no message in exception (getMessage() returned null)
         //
      }
  }

  @Test(timeout = 100000)
  public void test13()  throws Throwable  {
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader((JsonElement) null);
      // Undeclared exception!
      try { 
        jsonTreeReader0.nextString();
        fail("Expecting exception: AssertionError");
      
      } catch(AssertionError e) {
         //
         // no message in exception (getMessage() returned null)
         //
      }
  }

  @Test(timeout = 100000)
  public void test14()  throws Throwable  {
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader((JsonElement) null);
      // Undeclared exception!
      try { 
        jsonTreeReader0.nextNull();
        fail("Expecting exception: AssertionError");
      
      } catch(AssertionError e) {
         //
         // no message in exception (getMessage() returned null)
         //
      }
  }

  @Test(timeout = 100000)
  public void test15()  throws Throwable  {
      JsonObject jsonObject0 = new JsonObject();
      Boolean boolean0 = Boolean.valueOf("\"n(nIuf!c~WOSt$;#{K");
      jsonObject0.addProperty("\"n(nIuf!c~WOSt$;#{K", boolean0);
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonObject0);
      jsonTreeReader0.beginObject();
      JsonPrimitive jsonPrimitive0 = new JsonPrimitive("Expected a ");
      jsonObject0.add("Expected a ", jsonPrimitive0);
      // Undeclared exception!
      try { 
        jsonTreeReader0.nextName();
        fail("Expecting exception: ConcurrentModificationException");
      
      } catch(ConcurrentModificationException e) {
         //
         // no message in exception (getMessage() returned null)
         //
         verifyException("com.google.gson.internal.LinkedTreeMap$LinkedTreeMapIterator", e);
      }
  }

  @Test(timeout = 100000)
  public void test16()  throws Throwable  {
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader((JsonElement) null);
      // Undeclared exception!
      try { 
        jsonTreeReader0.nextName();
        fail("Expecting exception: AssertionError");
      
      } catch(AssertionError e) {
         //
         // no message in exception (getMessage() returned null)
         //
      }
  }

  @Test(timeout = 100000)
  public void test17()  throws Throwable  {
      JsonArray jsonArray0 = new JsonArray();
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonArray0);
      jsonTreeReader0.beginArray();
      jsonArray0.add(")?t[D?7lDYG,0{NJhu");
      // Undeclared exception!
      try { 
        jsonTreeReader0.nextLong();
        fail("Expecting exception: ConcurrentModificationException");
      
      } catch(ConcurrentModificationException e) {
         //
         // no message in exception (getMessage() returned null)
         //
         verifyException("java.util.ArrayList$Itr", e);
      }
  }

  @Test(timeout = 100000)
  public void test18()  throws Throwable  {
      JsonPrimitive jsonPrimitive0 = new JsonPrimitive("");
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonPrimitive0);
      // Undeclared exception!
      try { 
        jsonTreeReader0.nextLong();
        fail("Expecting exception: NumberFormatException");
      
      } catch(NumberFormatException e) {
         //
         // For input string: \"\"
         //
         verifyException("java.lang.NumberFormatException", e);
      }
  }

  @Test(timeout = 100000)
  public void test19()  throws Throwable  {
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader((JsonElement) null);
      // Undeclared exception!
      try { 
        jsonTreeReader0.nextLong();
        fail("Expecting exception: AssertionError");
      
      } catch(AssertionError e) {
         //
         // no message in exception (getMessage() returned null)
         //
      }
  }

  @Test(timeout = 100000)
  public void test20()  throws Throwable  {
      JsonPrimitive jsonPrimitive0 = new JsonPrimitive("com.google.gson.inter=al.bind.JsonTreeReader$1");
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonPrimitive0);
      // Undeclared exception!
      try { 
        jsonTreeReader0.nextInt();
        fail("Expecting exception: NumberFormatException");
      
      } catch(NumberFormatException e) {
         //
         // For input string: \"com.google.gson.inter=al.bind.JsonTreeReader$1\"
         //
         verifyException("java.lang.NumberFormatException", e);
      }
  }

  @Test(timeout = 100000)
  public void test21()  throws Throwable  {
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader((JsonElement) null);
      // Undeclared exception!
      try { 
        jsonTreeReader0.nextInt();
        fail("Expecting exception: AssertionError");
      
      } catch(AssertionError e) {
         //
         // no message in exception (getMessage() returned null)
         //
      }
  }

  @Test(timeout = 100000)
  public void test22()  throws Throwable  {
      JsonPrimitive jsonPrimitive0 = new JsonPrimitive("com.google.gson.internal.bind.JsonTreeReader$1");
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonPrimitive0);
      // Undeclared exception!
      try { 
        jsonTreeReader0.nextDouble();
        fail("Expecting exception: NumberFormatException");
      
      } catch(NumberFormatException e) {
      }
  }

  @Test(timeout = 100000)
  public void test23()  throws Throwable  {
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader((JsonElement) null);
      // Undeclared exception!
      try { 
        jsonTreeReader0.nextDouble();
        fail("Expecting exception: AssertionError");
      
      } catch(AssertionError e) {
         //
         // no message in exception (getMessage() returned null)
         //
      }
  }

  @Test(timeout = 100000)
  public void test24()  throws Throwable  {
      JsonArray jsonArray0 = new JsonArray();
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonArray0);
      jsonTreeReader0.beginArray();
      jsonArray0.add(")?t[D?7lDYG,0{NJhu");
      // Undeclared exception!
      try { 
        jsonTreeReader0.nextBoolean();
        fail("Expecting exception: ConcurrentModificationException");
      
      } catch(ConcurrentModificationException e) {
         //
         // no message in exception (getMessage() returned null)
         //
         verifyException("java.util.ArrayList$Itr", e);
      }
  }

  @Test(timeout = 100000)
  public void test25()  throws Throwable  {
      JsonNull jsonNull0 = JsonNull.INSTANCE;
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonNull0);
      // Undeclared exception!
      try { 
        jsonTreeReader0.nextBoolean();
        fail("Expecting exception: IllegalStateException");
      
      } catch(IllegalStateException e) {
         //
         // Expected BOOLEAN but was NULL at path $
         //
         verifyException("com.google.gson.internal.bind.JsonTreeReader", e);
      }
  }

  @Test(timeout = 100000)
  public void test26()  throws Throwable  {
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader((JsonElement) null);
      // Undeclared exception!
      try { 
        jsonTreeReader0.nextBoolean();
        fail("Expecting exception: AssertionError");
      
      } catch(AssertionError e) {
         //
         // no message in exception (getMessage() returned null)
         //
      }
  }

  @Test(timeout = 100000)
  public void test27()  throws Throwable  {
      JsonObject jsonObject0 = new JsonObject();
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonObject0);
      jsonTreeReader0.close();
      // Undeclared exception!
      try { 
        jsonTreeReader0.hasNext();
        fail("Expecting exception: IllegalStateException");
      
      } catch(IllegalStateException e) {
         //
         // JsonReader is closed
         //
         verifyException("com.google.gson.internal.bind.JsonTreeReader", e);
      }
  }

  @Test(timeout = 100000)
  public void test28()  throws Throwable  {
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader((JsonElement) null);
      // Undeclared exception!
      try { 
        jsonTreeReader0.hasNext();
        fail("Expecting exception: AssertionError");
      
      } catch(AssertionError e) {
         //
         // no message in exception (getMessage() returned null)
         //
      }
  }

  @Test(timeout = 100000)
  public void test29()  throws Throwable  {
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader((JsonElement) null);
      // Undeclared exception!
      try { 
        jsonTreeReader0.endObject();
        fail("Expecting exception: AssertionError");
      
      } catch(AssertionError e) {
         //
         // no message in exception (getMessage() returned null)
         //
      }
  }

  @Test(timeout = 100000)
  public void test30()  throws Throwable  {
      JsonArray jsonArray0 = new JsonArray();
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonArray0);
      jsonTreeReader0.beginArray();
      jsonArray0.add(")?t[D?7lDYG,0{NJhu");
      // Undeclared exception!
      try { 
        jsonTreeReader0.endArray();
        fail("Expecting exception: ConcurrentModificationException");
      
      } catch(ConcurrentModificationException e) {
         //
         // no message in exception (getMessage() returned null)
         //
         verifyException("java.util.ArrayList$Itr", e);
      }
  }

  @Test(timeout = 100000)
  public void test31()  throws Throwable  {
      JsonArray jsonArray0 = new JsonArray();
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonArray0);
      // Undeclared exception!
      try { 
        jsonTreeReader0.endArray();
        fail("Expecting exception: IllegalStateException");
      
      } catch(IllegalStateException e) {
         //
         // Expected END_ARRAY but was BEGIN_ARRAY at path $
         //
         verifyException("com.google.gson.internal.bind.JsonTreeReader", e);
      }
  }

  @Test(timeout = 100000)
  public void test32()  throws Throwable  {
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader((JsonElement) null);
      // Undeclared exception!
      try { 
        jsonTreeReader0.endArray();
        fail("Expecting exception: AssertionError");
      
      } catch(AssertionError e) {
         //
         // no message in exception (getMessage() returned null)
         //
      }
  }

  @Test(timeout = 100000)
  public void test33()  throws Throwable  {
      JsonNull jsonNull0 = JsonNull.INSTANCE;
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonNull0);
      // Undeclared exception!
      try { 
        jsonTreeReader0.beginObject();
        fail("Expecting exception: IllegalStateException");
      
      } catch(IllegalStateException e) {
         //
         // Expected BEGIN_OBJECT but was NULL at path $
         //
         verifyException("com.google.gson.internal.bind.JsonTreeReader", e);
      }
  }

  @Test(timeout = 100000)
  public void test34()  throws Throwable  {
      JsonObject jsonObject0 = new JsonObject();
      Byte byte0 = new Byte((byte)0);
      jsonObject0.addProperty(")@)<q>r 4@|R", (Number) byte0);
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonObject0);
      jsonTreeReader0.beginObject();
      jsonTreeReader0.skipValue();
      String string0 = jsonTreeReader0.getPath();
      assertEquals("$.null", string0);
  }

  @Test(timeout = 100000)
  public void test35()  throws Throwable  {
      JsonObject jsonObject0 = new JsonObject();
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonObject0);
      jsonTreeReader0.beginObject();
      String string0 = jsonTreeReader0.getPath();
      assertEquals("$.", string0);
  }

  @Test(timeout = 100000)
  public void test36()  throws Throwable  {
      JsonObject jsonObject0 = new JsonObject();
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonObject0);
      String string0 = jsonTreeReader0.getPath();
      assertEquals("$", string0);
  }

  @Test(timeout = 100000)
  public void test37()  throws Throwable  {
      JsonArray jsonArray0 = new JsonArray();
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonArray0);
      jsonTreeReader0.close();
      // Undeclared exception!
      try { 
        jsonTreeReader0.peek();
        fail("Expecting exception: IllegalStateException");
      
      } catch(IllegalStateException e) {
         //
         // JsonReader is closed
         //
         verifyException("com.google.gson.internal.bind.JsonTreeReader", e);
      }
  }

  @Test(timeout = 100000)
  public void test38()  throws Throwable  {
      JsonNull jsonNull0 = JsonNull.INSTANCE;
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonNull0);
      JsonToken jsonToken0 = jsonTreeReader0.peek();
      assertEquals(JsonToken.NULL, jsonToken0);
  }

  @Test(timeout = 100000)
  public void test39()  throws Throwable  {
      Short short0 = new Short((short) (-5371));
      JsonPrimitive jsonPrimitive0 = new JsonPrimitive(short0);
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonPrimitive0);
      JsonToken jsonToken0 = jsonTreeReader0.peek();
      assertEquals(JsonToken.NUMBER, jsonToken0);
  }

  @Test(timeout = 100000)
  public void test40()  throws Throwable  {
      Character character0 = new Character('`');
      JsonPrimitive jsonPrimitive0 = new JsonPrimitive(character0);
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonPrimitive0);
      JsonToken jsonToken0 = jsonTreeReader0.peek();
      assertEquals(JsonToken.STRING, jsonToken0);
  }

  @Test(timeout = 100000)
  public void test41()  throws Throwable  {
      Boolean boolean0 = new Boolean(" atUpath ");
      JsonPrimitive jsonPrimitive0 = new JsonPrimitive(boolean0);
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonPrimitive0);
      JsonToken jsonToken0 = jsonTreeReader0.peek();
      assertEquals(JsonToken.BOOLEAN, jsonToken0);
  }

  @Test(timeout = 100000)
  public void test42()  throws Throwable  {
      JsonArray jsonArray0 = new JsonArray(112);
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonArray0);
      JsonToken jsonToken0 = jsonTreeReader0.peek();
      assertEquals(JsonToken.BEGIN_ARRAY, jsonToken0);
  }

  @Test(timeout = 100000)
  public void test43()  throws Throwable  {
      JsonObject jsonObject0 = new JsonObject();
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonObject0);
      JsonToken jsonToken0 = jsonTreeReader0.peek();
      assertEquals(JsonToken.BEGIN_OBJECT, jsonToken0);
  }

  @Test(timeout = 100000)
  public void test44()  throws Throwable  {
      JsonArray jsonArray0 = new JsonArray();
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonArray0);
      jsonTreeReader0.beginArray();
      JsonToken jsonToken0 = jsonTreeReader0.peek();
      assertEquals(JsonToken.END_ARRAY, jsonToken0);
  }

  @Test(timeout = 100000)
  public void test45()  throws Throwable  {
      JsonObject jsonObject0 = new JsonObject();
      Byte byte0 = new Byte((byte)0);
      jsonObject0.addProperty(")@)<q>r 4@|R", (Number) byte0);
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonObject0);
      jsonTreeReader0.beginObject();
      JsonToken jsonToken0 = jsonTreeReader0.peek();
      assertEquals(JsonToken.NAME, jsonToken0);
  }

  @Test(timeout = 100000)
  public void test46()  throws Throwable  {
      JsonObject jsonObject0 = new JsonObject();
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonObject0);
      jsonTreeReader0.beginObject();
      JsonToken jsonToken0 = jsonTreeReader0.peek();
      assertEquals(JsonToken.END_OBJECT, jsonToken0);
  }

  @Test(timeout = 100000)
  public void test47()  throws Throwable  {
      JsonPrimitive jsonPrimitive0 = new JsonPrimitive("");
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonPrimitive0);
      jsonTreeReader0.nextString();
      JsonToken jsonToken0 = jsonTreeReader0.peek();
      assertEquals(JsonToken.END_DOCUMENT, jsonToken0);
  }

  @Test(timeout = 100000)
  public void test48()  throws Throwable  {
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader((JsonElement) null);
      // Undeclared exception!
      try { 
        jsonTreeReader0.peek();
        fail("Expecting exception: AssertionError");
      
      } catch(AssertionError e) {
         //
         // no message in exception (getMessage() returned null)
         //
      }
  }

  @Test(timeout = 100000)
  public void test49()  throws Throwable  {
      JsonObject jsonObject0 = new JsonObject();
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonObject0);
      jsonTreeReader0.beginObject();
      jsonTreeReader0.endObject();
      assertFalse(jsonTreeReader0.isLenient());
  }

  @Test(timeout = 100000)
  public void test50()  throws Throwable  {
      JsonObject jsonObject0 = new JsonObject();
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonObject0);
      // Undeclared exception!
      try { 
        jsonTreeReader0.nextName();
        fail("Expecting exception: IllegalStateException");
      
      } catch(IllegalStateException e) {
         //
         // Expected NAME but was BEGIN_OBJECT at path $
         //
         verifyException("com.google.gson.internal.bind.JsonTreeReader", e);
      }
  }

  @Test(timeout = 100000)
  public void test51()  throws Throwable  {
      Boolean boolean0 = new Boolean("I<Fjsn G<Zh}");
      JsonObject jsonObject0 = new JsonObject();
      jsonObject0.addProperty("I<Fjsn G<Zh}", boolean0);
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonObject0);
      jsonTreeReader0.beginObject();
      jsonTreeReader0.skipValue();
      // Undeclared exception!
      try { 
        jsonTreeReader0.nextNull();
        fail("Expecting exception: IllegalStateException");
      
      } catch(IllegalStateException e) {
         //
         // Expected NULL but was BOOLEAN at path $.null
         //
         verifyException("com.google.gson.internal.bind.JsonTreeReader", e);
      }
  }

  @Test(timeout = 100000)
  public void test52()  throws Throwable  {
      JsonObject jsonObject0 = new JsonObject();
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonObject0);
      jsonObject0.addProperty("", "");
      jsonTreeReader0.beginObject();
      // Undeclared exception!
      try { 
        jsonTreeReader0.endObject();
        fail("Expecting exception: IllegalStateException");
      
      } catch(IllegalStateException e) {
         //
         // Expected END_OBJECT but was NAME at path $.
         //
         verifyException("com.google.gson.internal.bind.JsonTreeReader", e);
      }
  }

  @Test(timeout = 100000)
  public void test53()  throws Throwable  {
      JsonArray jsonArray0 = new JsonArray();
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonArray0);
      jsonTreeReader0.beginArray();
      String string0 = jsonTreeReader0.getPath();
      assertEquals("$[0]", string0);
  }

  @Test(timeout = 100000)
  public void test54()  throws Throwable  {
      JsonArray jsonArray0 = new JsonArray();
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonArray0);
      String string0 = jsonTreeReader0.getPath();
      assertEquals("$", string0);
  }

  @Test(timeout = 100000)
  public void test55()  throws Throwable  {
      JsonObject jsonObject0 = new JsonObject();
      Boolean boolean0 = Boolean.FALSE;
      jsonObject0.addProperty("qXB", boolean0);
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonObject0);
      jsonTreeReader0.beginObject();
      jsonTreeReader0.promoteNameToValue();
      jsonTreeReader0.skipValue();
      assertFalse(jsonTreeReader0.isLenient());
  }

  @Test(timeout = 100000)
  public void test56()  throws Throwable  {
      JsonObject jsonObject0 = new JsonObject();
      Boolean boolean0 = Boolean.TRUE;
      jsonObject0.addProperty("2", boolean0);
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonObject0);
      jsonTreeReader0.beginObject();
      jsonTreeReader0.promoteNameToValue();
      int int0 = jsonTreeReader0.nextInt();
      assertEquals(2, int0);
  }

  @Test(timeout = 100000)
  public void test57()  throws Throwable  {
      JsonNull jsonNull0 = JsonNull.INSTANCE;
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonNull0);
      // Undeclared exception!
      try { 
        jsonTreeReader0.nextInt();
        fail("Expecting exception: IllegalStateException");
      
      } catch(IllegalStateException e) {
         //
         // Expected NUMBER but was NULL at path $
         //
         verifyException("com.google.gson.internal.bind.JsonTreeReader", e);
      }
  }

  @Test(timeout = 100000)
  public void test58()  throws Throwable  {
      JsonObject jsonObject0 = new JsonObject();
      Boolean boolean0 = Boolean.FALSE;
      jsonObject0.addProperty("+1", boolean0);
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonObject0);
      jsonTreeReader0.beginObject();
      jsonTreeReader0.promoteNameToValue();
      long long0 = jsonTreeReader0.nextLong();
      assertEquals(1L, long0);
  }

  @Test(timeout = 100000)
  public void test59()  throws Throwable  {
      Long long0 = new Long(0L);
      JsonPrimitive jsonPrimitive0 = new JsonPrimitive(long0);
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonPrimitive0);
      long long1 = jsonTreeReader0.nextLong();
      assertEquals(0L, long1);
  }

  @Test(timeout = 100000)
  public void test60()  throws Throwable  {
      JsonObject jsonObject0 = new JsonObject();
      Boolean boolean0 = Boolean.FALSE;
      jsonObject0.addProperty("+1", boolean0);
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonObject0);
      jsonTreeReader0.beginObject();
      jsonTreeReader0.promoteNameToValue();
      double double0 = jsonTreeReader0.nextDouble();
      assertEquals(1.0, double0, 0.01);
  }

  @Test(timeout = 100000)
  public void test61()  throws Throwable  {
      JsonPrimitive jsonPrimitive0 = new JsonPrimitive("NaN");
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonPrimitive0);
      // Undeclared exception!
      try { 
        jsonTreeReader0.nextDouble();
        fail("Expecting exception: NumberFormatException");
      
      } catch(NumberFormatException e) {
         //
         // JSON forbids NaN and infinities: NaN
         //
         verifyException("com.google.gson.internal.bind.JsonTreeReader", e);
      }
  }

  @Test(timeout = 100000)
  public void test62()  throws Throwable  {
      JsonPrimitive jsonPrimitive0 = new JsonPrimitive("2");
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonPrimitive0);
      jsonTreeReader0.setLenient(true);
      double double0 = jsonTreeReader0.nextDouble();
      assertEquals(2.0, double0, 0.01);
  }

  @Test(timeout = 100000)
  public void test63()  throws Throwable  {
      JsonNull jsonNull0 = JsonNull.INSTANCE;
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonNull0);
      // Undeclared exception!
      try { 
        jsonTreeReader0.nextDouble();
        fail("Expecting exception: IllegalStateException");
      
      } catch(IllegalStateException e) {
         //
         // Expected NUMBER but was NULL at path $
         //
         verifyException("com.google.gson.internal.bind.JsonTreeReader", e);
      }
  }

  @Test(timeout = 100000)
  public void test64()  throws Throwable  {
      JsonObject jsonObject0 = new JsonObject();
      jsonObject0.addProperty("|In5@d)}ur^Dqyq%z", (Boolean) null);
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonObject0);
      jsonTreeReader0.beginObject();
      jsonTreeReader0.skipValue();
      jsonTreeReader0.nextNull();
      assertFalse(jsonTreeReader0.isLenient());
  }

  @Test(timeout = 100000)
  public void test65()  throws Throwable  {
      Boolean boolean0 = Boolean.TRUE;
      JsonPrimitive jsonPrimitive0 = new JsonPrimitive(boolean0);
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonPrimitive0);
      boolean boolean1 = jsonTreeReader0.nextBoolean();
      assertTrue(boolean1);
  }

  @Test(timeout = 100000)
  public void test66()  throws Throwable  {
      Long long0 = new Long(2887L);
      JsonPrimitive jsonPrimitive0 = new JsonPrimitive(long0);
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonPrimitive0);
      String string0 = jsonTreeReader0.nextString();
      assertNotNull(string0);
  }

  @Test(timeout = 100000)
  public void test67()  throws Throwable  {
      JsonNull jsonNull0 = JsonNull.INSTANCE;
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonNull0);
      // Undeclared exception!
      try { 
        jsonTreeReader0.nextString();
        fail("Expecting exception: IllegalStateException");
      
      } catch(IllegalStateException e) {
         //
         // Expected STRING but was NULL at path $
         //
         verifyException("com.google.gson.internal.bind.JsonTreeReader", e);
      }
  }

  @Test(timeout = 100000)
  public void test68()  throws Throwable  {
      JsonObject jsonObject0 = new JsonObject();
      Boolean boolean0 = Boolean.FALSE;
      jsonObject0.addProperty("com.google.gson.internal.bind.JsonTreeReader", boolean0);
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonObject0);
      jsonTreeReader0.beginObject();
      jsonTreeReader0.promoteNameToValue();
      String string0 = jsonTreeReader0.nextString();
      assertEquals("com.google.gson.internal.bind.JsonTreeReader", string0);
  }

  @Test(timeout = 100000)
  public void test69()  throws Throwable  {
      Long long0 = new Long(0L);
      JsonPrimitive jsonPrimitive0 = new JsonPrimitive(long0);
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonPrimitive0);
      double double0 = jsonTreeReader0.nextDouble();
      assertEquals(0.0, double0, 0.01);
  }

  @Test(timeout = 100000)
  public void test70()  throws Throwable  {
      Boolean boolean0 = Boolean.TRUE;
      JsonObject jsonObject0 = new JsonObject();
      jsonObject0.addProperty("", boolean0);
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonObject0);
      jsonTreeReader0.beginObject();
      jsonTreeReader0.skipValue();
      boolean boolean1 = jsonTreeReader0.nextBoolean();
      assertTrue(boolean1);
  }

  @Test(timeout = 100000)
  public void test71()  throws Throwable  {
      JsonArray jsonArray0 = new JsonArray();
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonArray0);
      jsonTreeReader0.beginArray();
      jsonArray0.add("null");
      // Undeclared exception!
      try { 
        jsonTreeReader0.beginObject();
        fail("Expecting exception: ConcurrentModificationException");
      
      } catch(ConcurrentModificationException e) {
         //
         // no message in exception (getMessage() returned null)
         //
         verifyException("java.util.ArrayList$Itr", e);
      }
  }

  @Test(timeout = 100000)
  public void test72()  throws Throwable  {
      JsonNull jsonNull0 = JsonNull.INSTANCE;
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonNull0);
      jsonTreeReader0.nextNull();
      // Undeclared exception!
      try { 
        jsonTreeReader0.nextLong();
        fail("Expecting exception: IllegalStateException");
      
      } catch(IllegalStateException e) {
         //
         // Expected NUMBER but was END_DOCUMENT at path $
         //
         verifyException("com.google.gson.internal.bind.JsonTreeReader", e);
      }
  }

  @Test(timeout = 100000)
  public void test73()  throws Throwable  {
      JsonArray jsonArray0 = new JsonArray();
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonArray0);
      jsonTreeReader0.beginArray();
      boolean boolean0 = jsonTreeReader0.hasNext();
      assertFalse(boolean0);
  }

  @Test(timeout = 100000)
  public void test74()  throws Throwable  {
      JsonArray jsonArray0 = new JsonArray();
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonArray0);
      boolean boolean0 = jsonTreeReader0.hasNext();
      assertTrue(boolean0);
  }

  @Test(timeout = 100000)
  public void test75()  throws Throwable  {
      JsonObject jsonObject0 = new JsonObject();
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonObject0);
      jsonTreeReader0.beginObject();
      boolean boolean0 = jsonTreeReader0.hasNext();
      assertFalse(boolean0);
  }

  @Test(timeout = 100000)
  public void test76()  throws Throwable  {
      JsonArray jsonArray0 = new JsonArray();
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonArray0);
      jsonTreeReader0.beginArray();
      jsonTreeReader0.endArray();
      assertFalse(jsonTreeReader0.isLenient());
  }

  @Test(timeout = 100000)
  public void test77()  throws Throwable  {
      JsonArray jsonArray0 = new JsonArray();
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonArray0);
      String string0 = jsonTreeReader0.toString();
      assertEquals("JsonTreeReader", string0);
  }

  @Test(timeout = 100000)
  public void test78()  throws Throwable  {
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader((JsonElement) null);
      // Undeclared exception!
      try { 
        jsonTreeReader0.beginObject();
        fail("Expecting exception: AssertionError");
      
      } catch(AssertionError e) {
         //
         // no message in exception (getMessage() returned null)
         //
      }
  }

  @Test(timeout = 100000)
  public void test79()  throws Throwable  {
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader((JsonElement) null);
      // Undeclared exception!
      try { 
        jsonTreeReader0.beginArray();
        fail("Expecting exception: AssertionError");
      
      } catch(AssertionError e) {
         //
         // no message in exception (getMessage() returned null)
         //
      }
  }

  @Test(timeout = 100000)
  public void test80()  throws Throwable  {
      JsonObject jsonObject0 = new JsonObject();
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonObject0);
      // Undeclared exception!
      try { 
        jsonTreeReader0.promoteNameToValue();
        fail("Expecting exception: IllegalStateException");
      
      } catch(IllegalStateException e) {
         //
         // Expected NAME but was BEGIN_OBJECT at path $
         //
         verifyException("com.google.gson.internal.bind.JsonTreeReader", e);
      }
  }

  @Test(timeout = 100000)
  public void test81()  throws Throwable  {
      JsonObject jsonObject0 = new JsonObject();
      JsonTreeReader jsonTreeReader0 = new JsonTreeReader(jsonObject0);
      jsonTreeReader0.close();
      // Undeclared exception!
      try { 
        jsonTreeReader0.skipValue();
        fail("Expecting exception: IllegalStateException");
      
      } catch(IllegalStateException e) {
         //
         // JsonReader is closed
         //
         verifyException("com.google.gson.internal.bind.JsonTreeReader", e);
      }
  }
}