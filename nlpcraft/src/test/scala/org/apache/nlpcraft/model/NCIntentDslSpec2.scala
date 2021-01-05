/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.nlpcraft.model

import org.apache.nlpcraft.{NCTestContext, NCTestEnvironment}
import org.junit.jupiter.api.Assertions.{assertEquals, assertTrue}
import org.junit.jupiter.api.Test

import java.util
import scala.collection.JavaConverters._
import scala.language.implicitConversions

/**
  * Intents DSL test model.
  */
class NCIntentDslSpecModel2 extends NCModelAdapter(
    "nlpcraft.intents.dsl.test", "Intents DSL Test Model", "1.0"
) {
    private implicit def convert(s: String): NCResult = NCResult.text(s)

    override def getElements: util.Set[NCElement] =
        Set("a").map(id ⇒ new NCElement { override def getId: String = id }).asJava

    @NCIntent("intent=a_11 term(a)={id == 'a'}")
    private def a11(ctx: NCIntentMatch): NCResult = "OK"

    @NCIntent("intent=a_23 term(a)={id == 'a'}[2,3]")
    private def a23(ctx: NCIntentMatch): NCResult = "OK"

    @NCIntent("intent=a_15 term(a)={id == 'a'}[1,5]")
    private def a15(ctx: NCIntentMatch): NCResult = "OK"

    @NCIntent("intent=a_plus term(a)={id == 'a'}+")
    private def a1Inf(ctx: NCIntentMatch): NCResult = "OK"

    @NCIntent("intent=a_star term(a)={id == 'a'}*")
    private def a0Inf(ctx: NCIntentMatch): NCResult = "OK"
}

/**
  * Intents DSL test.
  */
@NCTestEnvironment(model = classOf[NCIntentDslSpecModel2], startClient = true)
class NCIntentDslSpec2 extends NCTestContext {
    private def check(txt: String, intent: String): Unit = {
        val res = getClient.ask(txt)

        assertTrue(res.isOk, s"Checked: $txt")
        assertTrue(res.getResult.isPresent, s"Checked: $txt")
        assertEquals(intent, res.getIntentId, s"Checked: $txt")
    }

    @Test
    def test(): Unit = {
        check("a", "a_11")
        check("a a", "a_23")
        check("a a a", "a_23")
        check("a a a a", "a_15")
        check("a a a a a a ", "a_plus")
    }
}
