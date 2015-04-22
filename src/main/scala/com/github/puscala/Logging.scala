package com.github.puscala

import org.slf4j.LoggerFactory

private[puscala] trait Logging {

  lazy val loggerName = this.getClass.getName

  def logger = LoggerFactory.getLogger(loggerName)

  /**
   * output trace log
   * @param message
   */
  def trace(message: => String) = logger.trace(message)

  /**
   * output trace log with throwable
   * @param message
   * @param error
   */
  def trace(message: => String, error: => Throwable) = logger.trace(message, error)

  /**
   * output debug log
   * @param message
   */
  def debug(message: => String) = logger.debug(message)

  /**
   * output debug log with throwable
   * @param message
   * @param error
   */
  def debug(message: => String, error: => Throwable) = logger.debug(message, error)

  /**
   * output info log
   * @param message
   */
  def info(message: => String) = logger.info(message)

  /**
   * output info log with throwable
   * @param message
   * @param error
   */
  def info(message: => String, error: => Throwable) = logger.info(message, error)

  /**
   * output warn log
   * @param message
   */
  def warn(message: => String) = logger.warn(message)

  /**
   * output warn log with throwable
   * @param message
   * @param error
   */
  def warn(message: => String, error: => Throwable) = logger.warn(message, error)

  /**
   * output error log
   * @param message
   */
  def error(message: => String) = logger.error(message)

  /**
   * output error log with throwable
   * @param message
   * @param error
   */
  def error(message: => String, error: => Throwable) = logger.error(message, error)

}
