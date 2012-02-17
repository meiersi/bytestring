/* Variable-length, little-endian, base-128 encoding adapted by
 * Simon Meier <iridcode@gmail.com> from the upb library:
 * https://github.com/haberman/upb
 *
 * ***************************************************************************
 * upb - a minimalist implementation of protocol buffers.
 *
 * Author: Josh Haberman <jhaberman@gmail.com>
 *
 * Copyright (c) 2009-2011, Google Inc.
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of Google Inc. nor the names of any other
 *       contributors may be used to endorse or promote products
 *       derived from this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY GOOGLE INC. ``AS IS'' AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
 * EVENT SHALL GOOGLE INC. BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
 * IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 * 
 */

// Encodes a 64-bit varint into buf and returns the pointer to the next free
// byte.
char* _hs_bytestring_word64Base128LE (unsigned long long int val, char *buf) {
  if (val == 0) { *buf = 0; return buf + 1; }
  while (val) {
    unsigned char byte = val & 0x7f;
    val >>= 7;
    if (val) byte |= 0x80;
    *buf++ = byte;
  }
  return buf;
}

// Encodes a 32-bit varint into buf and returns the pointer to the next free
// byte.
char* _hs_bytestring_word32Base128LE (unsigned int val, char *buf) {
  if (val == 0) { *buf = 0; return buf + 1; }
  while (val) {
    unsigned char byte = val & 0x7f;
    val >>= 7;
    if (val) byte |= 0x80;
    *buf++ = byte;
  }
  return buf;
}
