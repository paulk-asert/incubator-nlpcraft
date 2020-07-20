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

package org.apache.nlpcraft.model.tools.test

import com.typesafe.scalalogging.LazyLogging
import org.apache.nlpcraft.common.ascii.NCAsciiTable
import org.apache.nlpcraft.common.util.NCUtils
import org.apache.nlpcraft.model.intent.impl.NCIntentDslCompiler
import org.apache.nlpcraft.model.{NCIntent, NCIntentRef, NCIntentSample, NCModel}
import org.apache.nlpcraft.probe.embedded.NCEmbeddedProbe

/**
  * TODO: add description and check all outputs
  */
object NCSamplesModelValidator extends App with LazyLogging {
    // TODO: add desc
    private final val PROP_MDLS = "NLPCRAFT_TEST_MODELS"

    private final val CLS_SAMPLE = classOf[NCIntentSample]
    private final val CLS_INTENT = classOf[NCIntent]
    private final val CLS_INTENT_REF = classOf[NCIntentRef]

    @throws[Exception]
    def isValid: Boolean =
        NCUtils.sysEnv(PROP_MDLS) match {
            case Some(p) ⇒ isValid(getClasses(p))
            case None ⇒
                logger.warn(s"'$PROP_MDLS' is not defined")

                true
        }

    @throws[Exception]
    def isValid(claxx: Class[_ <: NCModel]): Boolean = isValid(Seq(claxx))

    @throws[Exception]
    def isValid(mdlId: String): Boolean = isValid(getClasses(mdlId))

    @throws[Exception]
    private def isValid(classes: Seq[Class[_ <: NCModel]]) = {
        val samples = getSamples(classes.map(cl ⇒ cl → cl.newInstance.getId).toMap)

        NCEmbeddedProbe.start(classes: _*)

        try
            process(samples)
        finally
            NCEmbeddedProbe.stop()
    }

    /**
      *
      * @param samples
      * @return
      */
    private def process(samples: Map[String, Map[String, Set[String]]]): Boolean = {
        case class Error(text: String, intentId: String)

        val errs: Map[String, Seq[Error]] =
            samples.filter(_._2.nonEmpty).flatMap { case (mdlId, mdlSamples) ⇒
                val cli = new NCTestClientBuilder().newBuilder.build

                cli.open(mdlId)

                try {
                    def getError(intentId: String, sample: String): Option[Error] = {
                        val res = cli.ask(sample)

                        if (res.isFailed)
                            Some(Error(s"Request '$sample' executed with error '${res.getResultError.get()}'", intentId))
                        else if (intentId != res.getIntentId)
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

            tbl #= ("Model ID", "Intent ID", "Error")

            for ((mdlId, errs) ← errs; err ← errs.sortBy(_.intentId))
                tbl += (mdlId, err.intentId, err.text)

            logger.error(s"Models intents samples errors: \n${tbl.toString}")

            false
        }
        else {
            val tbl = NCAsciiTable()

            tbl #= ("Model ID", "Intent ID", "Samples count")

            for ((mdlId, seq) ← samples; (intentId, samples) ← seq)
                tbl += (mdlId, intentId, samples.size)

            logger.info(s"Models intents samples tested: \n${tbl.toString}")

            true
        }
    }

    /**
      *
      * @param mdls
      * @return
      */
    private def getSamples(mdls: Map[Class[_ <: NCModel], String]): Map[String, Map[String, Set[String]]] =
        mdls.flatMap { case (claxx, mdlId) ⇒
            var annFound = false

            val mdlData =
                claxx.getDeclaredMethods.flatMap(method ⇒ {
                    val sample = method.getAnnotation(CLS_SAMPLE)
                    val intent = method.getAnnotation(CLS_INTENT)
                    val ref = method.getAnnotation(CLS_INTENT_REF)

                    if (sample != null || intent != null || ref != null) {
                        annFound = true

                        def getId: String =
                            if (intent != null)
                                NCIntentDslCompiler.compile(intent.value(),mdlId).id
                            else if (ref != null)
                                ref.value().trim
                            else
                                throw new AssertionError()

                        if (sample != null) {
                            if (intent == null && ref == null) {
                                logger.warn(s"Samples for nothing [method=${method.getName}]")

                                None
                            }
                            else {
                                val samples = sample.value().toSet

                                if (samples.isEmpty) {
                                    logger.warn(s"Samples are empty [intent=$getId, method=${method.getName}]")

                                    None
                                }
                                else
                                    Some(getId → samples)
                            }
                        }
                        else {
                            logger.warn(s"Samples are missed [intent=$getId, method=${method.getName}]")

                            None
                        }
                    }
                    else
                        None
                }).toMap

            if (mdlData.isEmpty) {
                if (!annFound)
                    logger.warn(s"Model '$mdlId' doesn't have intent annotations")

                None
            }
            else
                Some(mdlId → mdlData)
        }

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

    // All warnings printed to log.
    isValid
}
