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

import scala.collection.JavaConverters._

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
                val classes = getClasses(s)

                val intentSamples = getIntentSamples(classes)
                val mdlSamples = intentSamples.filter(_._2.isEmpty).keys.map(m ⇒ m → m.getExamples.asScala.toSet).toMap

                NCEmbeddedProbe.start(classes: _*)

                try {
                    processIntentsSamples(intentSamples.filter(_._2.nonEmpty))
                    processModelSamples(mdlSamples)
                }
                finally
                    NCEmbeddedProbe.stop()

            case None ⇒ System.err.println("'NLPCRAFT_TEST_MODELS' is not defined") // TODO: warn
        }

    /**
      *
      * @param samples
      */
    private def processIntentsSamples(samples: Map[NCModel, Map[String, Set[String]]]): Unit = {
        case class Error(text: String, intentId: String)

        val errs: Map[String, Seq[Error]] =
            samples.flatMap { case (mdl, mdlSamples) ⇒
                val mdlId = mdl.getId

                val cli = new NCTestClientBuilder().newBuilder.build

                cli.open(mdlId)

                try {
                    def getError(intentId: String, sample: String): Option[Error] = {
                        val res = cli.ask(sample)

                        if (res.isFailed)
                             // TODO:
                            Some(Error(s"Request '$sample' executed with error '${res.getResultError.get()}'", intentId))
                        else if (intentId != res.getIntentId)
                             // TODO:
                             Some(Error(s"Request '$sample' executed for unexpected intent ID '${res.getIntentId}'", intentId))
                        else
                             None
                    }

                    val mdlErrs =
                        (for ((intentId, samples) ← mdlSamples; sample ← samples)
                            yield getError(intentId, sample)).flatten

                    if (mdlErrs.nonEmpty) Some(mdlId → mdlErrs.toSeq) else None
                }
                finally
                    cli.close()
            }

        if (errs.nonEmpty) {
            val tbl = NCAsciiTable()

            // TODO: headers
            tbl #= ("Model ID", "Intent ID", "Error")

            for ((mdlId, errs) ← errs) {
                tbl += (mdlId, "", "")

                for (err ← errs.sortBy(_.intentId))
                    tbl += ("", err.intentId, err.text)
            }

            System.err.println("Models intents samples errors.")
            System.err.println(tbl.toString)

            Assertions.fail("See errors above")
        }
        else {
            val tbl = NCAsciiTable()

            // TODO: headers
            tbl #= ("Model ID", "Intent ID", "Samples count")

            for ((mdlId, seq) ← samples) {
                tbl += (mdlId, "", "")

                for ((intentId, samples) ← seq)
                    tbl += ("", intentId, samples.size)
            }

            println("Models intents samples tested.")
            println(tbl.toString)
        }
    }

    /**
      *
      * @param samples
      */
    private def processModelSamples(samples: Map[NCModel, Set[String]]): Unit = {
        val errs: Map[String, Set[String]] =
            samples.flatMap { case (mdl, mdlSamples) ⇒
                val mdlId = mdl.getId

                val cli = new NCTestClientBuilder().newBuilder.build

                cli.open(mdlId)

                try {
                    def getError(sample: String): Option[String] = {
                        val res = cli.ask(sample)

                        if (res.isFailed)
                            // TODO:
                            Some(s"Request '$sample' executed with error '${res.getResultError.get()}'")
                        else
                            None
                    }

                    val mdlErrs =
                        (for (sample ← mdlSamples)
                            yield getError(sample)).flatten

                    if (mdlErrs.nonEmpty) Some(mdlId → mdlErrs) else None
                }
                finally
                    cli.close()
            }

        if (errs.nonEmpty) {
            val tbl = NCAsciiTable()

            // TODO: headers
            tbl #= ("Model ID", "Error")

            for ((mdlId, errs) ← errs) {
                tbl += (mdlId,  "")

                for (err ← errs)
                    tbl += ("", err)
            }

            System.err.println("Models samples errors.")
            System.err.println(tbl.toString)

            Assertions.fail("See errors above")
        }
        else {
            val tbl = NCAsciiTable()

            // TODO: headers
            tbl #= ("Model ID", "Samples count")

            for ((mdl, samples) ← samples)
                tbl += (mdl.getId, samples.size)

            println("Models samples tested.")
            println(tbl.toString)
        }
    }

    /**
      *
      * @param classes
      * @return
      */
    private def getIntentSamples(classes: Seq[Class[_ <: NCModel]]): Map[NCModel, Map[String, Set[String]]] =
        classes.map(claxx ⇒ {
            val mdl = claxx.newInstance()
            val mdlId = mdl.getId

            val samples = claxx.getDeclaredMethods.flatMap(method ⇒ {
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

                    val seq = sample.value().toSet

                    if (seq.nonEmpty) Some(id → seq) else None
                }
                else
                    None
            }).toMap

            mdl → samples
        }).toMap

    /**
      *
      * @param s
      * @return
      */
    private def getClasses(s: String): Seq[Class[_ <: NCModel]] = {
        val clsLdr = Thread.currentThread().getContextClassLoader

        s.
            split(",").
            map(_.trim).
            filter(_.nonEmpty).
            map(clsLdr.loadClass).
            map(_.asSubclass(classOf[NCModel]))
    }
}
