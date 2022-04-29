Mock.hPutStrLn Mock.stdin s
output <-
  Mock.hGetLine Mock.stdout
when (...) (fail $ ...)
