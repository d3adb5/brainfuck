-- This module was taken from the imperative-edsl package. It was modified only
-- to make the fakeIO actio also embed the action's return in its final
-- statement.
--
-- The following is the copyright notice:
--
-- Copyright (c) 2022 d3adb5
-- Copyright (c) 2016 Emil Axelsson, Anton Ekblad, Máté Karácsony
-- Copyright (c) 2015 Anders Persson, Anton Ekblad, Emil Axelsson,
--                    Markus Aronsson, Josef Svenningsson
--
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--
--     * Redistributions in binary form must reproduce the above
--       copyright notice, this list of conditions and the following
--       disclaimer in the documentation and/or other materials provided
--       with the distribution.
--
--     * Neither the names of the copyright holders nor the names of other
--       contributors may be used to endorse or promote products derived
--       from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


-- | Running actions with explicit input\/output connected to
-- @`stdin`@\/@`stdout`@.
--
-- This module is inspired by the package <http://hackage.haskell.org/package/silently>.

module System.IO.Fake where

import Control.DeepSeq
import Control.Exception
import GHC.IO.Handle
import System.Directory
import System.IO

-- | Perform an action that with access to a temporary file. The file is removed
-- after the action is completed.
withTempFile
    :: FilePath                     -- ^ Path to directory for temporary file
    -> String                       -- ^ Base name for temporary file
    -> ((FilePath,Handle) -> IO a)  -- ^ Action
    -> IO a
withTempFile tmpDir base k = bracket
    (openTempFile tmpDir base)
    (\(file,h) -> hClose h >> removeFile file)
    k

-- | Perform an action with a redirected handle
withRedirect
    :: Handle  -- ^ Shadowing handle
    -> Handle  -- ^ Shadowed handle
    -> IO a    -- ^ Action in which the redirect takes place
    -> IO a
withRedirect new old act = bracket
    (do buffering <- hGetBuffering old
        dupH      <- hDuplicate old
        hDuplicateTo new old
        return (dupH,buffering)
    )
    (\(dupH,buffering) -> do
        hDuplicateTo dupH old
        hSetBuffering old buffering
        hClose dupH
    )
    (\_ -> act)

-- | Perform an action with explicit input\/output connected to
-- @`stdin`@\/@`stdout`@
fakeIO
    :: IO a            -- ^ Action
    -> String          -- ^ Input to send to @stdin@
    -> IO (a, String)  -- ^ Result from action and from @stdout@
fakeIO act inp = do
    tmpDir <- getTemporaryDirectory
    withTempFile tmpDir "fakeInput" $ \(_,inpH) ->
      withTempFile tmpDir "fakeOutput" $ \(_,outH) -> do
        withRedirect outH stdout $
          withRedirect inpH stdin $ do
            hPutStr inpH inp
            hSeek inpH AbsoluteSeek 0
            actReturn <- act
            hFlush stdout
            hSeek outH AbsoluteSeek 0
            str <- hGetContents outH
            str `deepseq` return (actReturn, str)
