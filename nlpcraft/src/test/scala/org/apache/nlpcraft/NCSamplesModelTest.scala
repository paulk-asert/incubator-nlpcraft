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

package org.apache.nlpcraft

import org.apache.nlpcraft.common.NCE
import org.apache.nlpcraft.common.ascii.NCAsciiTable
import org.apache.nlpcraft.common.util.NCUtils
import org.apache.nlpcraft.model.intent.impl.NCIntentDslCompiler
import org.apache.nlpcraft.model.tools.test.NCTestClientBuilder
import org.apache.nlpcraft.model.{NCIntent, NCIntentRef, NCIntentSample, NCModel}
import org.apache.nlpcraft.probe.embedded.NCEmbeddedProbe
import org.junit.jupiter.api.{Assertions, Test}

/**
  * TODO:
  */
class NCSamplesModelTest {
    private final val CLS_SAMPLE = classOf[NCIntentSample]
    private final val CLS_INTENT = classOf[NCIntent]
    private final val CLS_INTENT_REF = classOf[NCIntentRef]

    @Test
    @throws[Exception]
    def test(): Unit =
        // TODO: add desc
        NCUtils.sysEnv("NLPCRAFT_TEST_MODELS") match {
            case Some(s) ⇒
                val clsLdr = Thread.currentThread().getContextClassLoader

                val classes =
                    s.
                        split(",").
                        map(_.trim).
                        filter(_.nonEmpty).
                        map(clsLdr.loadClass).
                        map(_.asSubclass(classOf[NCModel]))

                val samples: Map[String, Map[String, Seq[String]]] = classes.map(claxx ⇒ {
                    val mdlId = claxx.newInstance().getId

                    mdlId →
                        claxx.getDeclaredMethods.flatMap(method ⇒ {
                            val sample = method.getAnnotation(CLS_SAMPLE)

                            if (sample != null) {
                                val cls = method.getAnnotation(CLS_INTENT)

                                val id =
                                    if (cls != null)
                                        NCIntentDslCompiler.compile(cls.value(), mdlId).id
                                    else {
                                        val ref = method.getAnnotation(CLS_INTENT_REF)

                                        if (ref == null)
                                            // TODO:
                                            throw new NCE(s"Model '$mdlId' has sample annotation but doesn't have intent or intent reference annotations")

                                        ref.value().trim
                                    }

                                val seq = sample.value().toSeq

                                if (seq.nonEmpty) Some(id → seq) else None
                            }
                            else
                                None
                        }).toMap
                }).toMap

                NCEmbeddedProbe.start(classes: _*)

                val errs =
                    try
                        samples.flatMap { case (mdlId, seq) ⇒
                            require(seq.nonEmpty)

                            val cli = new NCTestClientBuilder().newBuilder.build

                            cli.open(mdlId)

                            def getError(intentId: String, example: String): Option[String] = {
                                val res = cli.ask(example)

                                if (res.isFailed)
                                    // TODO:
                                    Some(s"Request '$example' executed with error '${res.getResultError.get()}'")
                                else if (intentId != res.getIntentId)
                                    // TODO:
                                    Some(s"Request '$example' executed for unexpected intent ID '${res.getIntentId}' instead of '$intentId'")
                                else {
                                    // TODO:
                                    println(s"Query: $example executed with result: ${res.getResult.get()} for intent: ${res.getIntentId}")

                                    None
                                }
                            }

                            try {
                                val mdlErrs =
                                    (for ((intentId, examples) ← seq; example ← examples) yield getError(intentId, example)).flatten

                                if (mdlErrs.nonEmpty) Some(mdlId → mdlErrs.toSeq) else None
                            }
                            finally
                                cli.close()
                        }
                    finally
                        NCEmbeddedProbe.stop()

                if (errs.nonEmpty) {
                    val tbl = NCAsciiTable()

                    // TODO: headers
                    tbl #= ("Model ID", "Error")

                    errs.foreach { case (mdlId, errs) ⇒
                        tbl += (mdlId, "")

                        errs.foreach(err ⇒ tbl += ("", err))
                    }

                    System.err.println("Models samples errors.")
                    System.err.println(tbl.toString)

                    Assertions.fail("See errors above")
                }
                else {
                    val tbl = NCAsciiTable()

                    // TODO: headers
                    tbl #= ("Model ID", "Samples count")

                    samples.foreach { case (mdlId, seq) ⇒ tbl += (mdlId, seq.size) }

                    println("Models samples tested.")
                    println(tbl.toString)
                }
            case None ⇒ System.err.println("'NLPCRAFT_TEST_MODELS' is not defined") // TODO: warn
        }
}
