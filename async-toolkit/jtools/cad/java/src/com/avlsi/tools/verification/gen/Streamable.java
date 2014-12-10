package com.avlsi.tools.verification.gen;

public interface Streamable {

   /**
   * Generate generates a stream of byte data and places it into buffer 
   * <code>buffer</code>.
   * 
   * @param buffer The buffer to be populated with the byte stream
   * @param offset The starting position within the buffer
   * @param length The maximum number of bytes to write to the buffer
   * 
   * @return The number of bytes written to the buffer
   **/
   int generate(byte[] buffer, int offset, int length);
} 
