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
    // TODO: add desc
    private final val PROP_MDLS = "NLPCRAFT_TEST_MODELS"

    private final val CLS_SAMPLE = classOf[NCIntentSample]
    private final val CLS_INTENT = classOf[NCIntent]
    private final val CLS_INTENT_REF = classOf[NCIntentRef]

    @Test
    @throws[Exception]
    def test(): Unit =
        NCUtils.sysEnv(PROP_MDLS) match {
            case Some(p) ⇒
                val classes = getClasses(p)
                val mdls = classes.map(cl ⇒ cl → cl.newInstance).toMap

                val intentSamples = getIntentSamples(mdls)
                val mdlSamples = getModelSamples(mdls, intentSamples)

                validate(mdls, intentSamples, mdlSamples)


                NCEmbeddedProbe.start(classes: _*)

                try {
                    processIntentsSamples(intentSamples)
                    processModelSamples(mdlSamples)
                }
                finally
                    NCEmbeddedProbe.stop()

            case None ⇒ System.err.println(s"'$PROP_MDLS' is not defined") // TODO: warn
        }

    def validate(
        mdls: Map[Class[_ <: NCModel], NCModel],
        intentSamples: Map[NCModel, Map[String, Set[String]]],
        mdlSamples: Map[NCModel, Set[String]]
    ): Unit = {
        // TODO:
    }

    /**
      *
      * @param mdls
      * @param intentSamples
      * @return
      */
    private def getModelSamples(
        mdls: Map[Class[_ <: NCModel], NCModel],
        intentSamples: Map[NCModel, Map[String, Set[String]]]
    ): Map[NCModel, Set[String]] = {
        mdls.values.flatMap(mdl ⇒ {
            val mdlSamples =
                mdl.getExamples.asScala.toSet.diff(intentSamples.getOrElse(mdl, Map.empty).values.flatten.toSet)

            if (mdlSamples.nonEmpty) Some(mdl → mdlSamples) else None
        }).toMap
    }

    /**
      *
      * @param samples
      */
    private def processIntentsSamples(samples: Map[NCModel, Map[String, Set[String]]]): Unit = {
        case class Error(text: String, intentId: String)

        val errs: Map[String, Seq[Error]] =
            samples.filter(_._2.nonEmpty).flatMap { case (mdl, mdlSamples) ⇒
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
      * @param mdls
      * @return
      */
    private def getIntentSamples(mdls: Map[Class[_ <: NCModel], NCModel]): Map[NCModel, Map[String, Set[String]]] =
        mdls.flatMap { case (claxx, mdl) ⇒
            val samples = claxx.getDeclaredMethods.flatMap(method ⇒ {
                val sample = method.getAnnotation(CLS_SAMPLE)

                if (sample != null) {
                    val cls = method.getAnnotation(CLS_INTENT)

                    val id =
                        if (cls != null)
                            NCIntentDslCompiler.compile(cls.value(), mdl.getId).id
                        else {
                            val ref = method.getAnnotation(CLS_INTENT_REF)

                            if (ref == null)
                                // TODO:
                                throw new NCE(
                                    s"Model '${mdl.getId}' has sample annotation but doesn't have intent or " +
                                        s"intent reference annotations"
                                )

                            ref.value().trim
                        }

                    val intSamples = sample.value().toSet

                    if (intSamples.nonEmpty) Some(id → intSamples) else None
                }
                else
                    None
            }).toMap

            if (samples.nonEmpty) Some(mdl → samples) else None
        }.toMap

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
