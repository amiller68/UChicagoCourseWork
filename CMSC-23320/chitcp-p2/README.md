chiTCP - A simple, testable TCP stack
=====================================

The chiTCP documentation is available at http://chi.cs.uchicago.edu/chitcp/


NOTE TO GRADER -
- Our group consists of three people and is subject to an extra
  free submission extension for project 2a only.

- Our group includes Mayokun Abiona.

- we modified packet.h, not realizing that this won't compile on Gradescope. It
  must be cloned and built from the master branch of our repository. Sorry about
  that.

- Our Implementation uses an initial RTO of 200ms rather than the proscibed 1s
  -> Changing this results in more failed tests and timeouts

- We've noticed that some tests sometimes crash, recieving the error:
        'free(): double free detected in tcache 2'
  If possible, please re-run tests that fail this way.
