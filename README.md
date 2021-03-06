# Test Server & Client

(Written by Eli Barzilay <eli@barzilay.org>.)

This is a server and a client to run a paperless test.  The clients will
run in "kiosk mode" --- locking out all other applications, which is
intended to prevent students from chatting, looking up stuff on the web
etc.  (Locking works only on Windows.)  The server can run in any
platform, but it is slightly more convenient if it is unix-like
(Linux/WSL/macOS), preferably inside Emacs.


## Why?

Running a paperless exam has some major advantages:

* When you get back from your exam you don't need to schlep a pile of
  papers.  You don't need to count them every five seconds to make sure
  that they're all there.  And you also don't need to worry about
  leaving the pile of graded exams on the roof of your car and realize
  later that you forgot them there, then spend three hours scanning the
  roads and sidewalks.

* You don't need to deal with student handwriting (sometime even
  intentionally obscured --- I've seen cases where students who didn't
  know the answer wrote text that could barely be read).

* You can provide the students with additional materials, so they don't
  need to kill half of a tree because your exam is open materials.
  (However, this is limited to text files.)

* You can update files during the test: clarify questions, add hints
  etc.  You can even have a rough sketch of some questions and write
  them up as the test goes on...

* You can provide "template files" for the students to modify.  For
  example, you can give them some function and ask to fix a bug.

* Grading is often a repetitive job, and I found myself starting with
  writing detailed comments, and later on (when the 10th exam has the
  same mistake) settle on explanation-less crosses and other obscure
  markings.  When you edit files, it is easy to copy+paste the same
  comment.

* Grading can be more consistent if you're doing such copy+paste that
  includes the grading.  In addition, it is easy to grep for a
  particular error that you've seen to make sure that the grading is
  consistent.

* Obviously, you can easily come up with a textual markup system so that
  you compute the exam grades automatically from the graded files.  I'm
  using a system like that for homework submission and grading, and
  having exams in electronic formats fits nicely into this system.

* The same holds for distributing the works.  You can keep the original
  pristine files in case students want to complain --- no need to worry
  about pencil-written exam being modified post-grading, and you can
  have the graded files available through your usual channels (eg,
  through the Racket handin-server).


## Features

The server and the client were designed to be *very* robust and secure,
while being convenient to use.

* Server-client communication is done through an encrypted connection.
  The server can be setup to accept connections only from some known IP
  addresses (and will report attempted connections from other IPs).

* Clients do not have a "save" or a "submit" button --- the text is
  continuously sent back to the server for storage.  This frees students
  from dealing with such details while they're stressed for time.

* Clients do *not* have a Racket REPL.  This wouldn't be too hard to
  add, but I think that this would be bad for several reasons: students
  can easily get dragged to a long debugging session instead of focusing
  on the test; you can get dragged to asking tougher questions since the
  students can now actually try things out; and you can get dragged to
  harsher grading ("if it doesn't work, you don't get the points",
  versus "you got the rough idea right, so you get the points" which is
  common with written exams and make them less stressful).

* The server backs up the directories of all clients periodically, to
  make things safer, and to avoid "I accidentally hit delete in the last
  minute" excuses.

* Clients use a number of tricks to make the application run in "kiosk
  mode": making it nearly impossible to switch to another application or
  kill the client.  There are cases when some of these may fail (for
  example, at the NEU lab ctrl+alt+delete cannot be disabled) --- the
  client has a safety mechanism against this: if you hit ctrl+alt+delete
  it will notice that it's not the focused window, and then it will (a)
  alert the server, and (b) set itself back into focus.

* You can set up a master password that can unlock a client.  (Hit
  ctrl+shift+P, then the password, then enter.)  This is another safety
  feature in case of some server catastrophe: in this case you can
  manually unlock a station and copy the written solution out to a
  different application.

* The server keeps a live connection with all clients and polls them
  frequently.  This is done for two reasons:

  - if the client dies (either due to a bug, or to a break attempt), the
    server will detect and report it,

  - on each poll the client sends the server the updated contents of
    editable buffers to save.

* The client can withstand being disconnected from the server (e.g., if
  the server is restarted for some reason) --- it will alert the
  student, but will continue to operate as usual.  (Specifically, the
  student can continue reading files and writing answers during such
  downtimes.)  The client keeps all changes and when a connection is
  re-established, synchronization resumes.

* You can update client-visible files on the server --- the changes will
  be sent to all clients, and the clients will visually indicate
  modified files so students can easily see that they need to read
  something.

* The server can require users to login to be able to edit files.  In
  this case, the files that the student writes will be saved in a
  directory with the username.  It can also work in a login-less mode,
  so each client corresponds to a single student working; in this case,
  you will need to make students write their names in their answer
  file(s).  (This sounds insecure, but works perfectly fine in practice:
  as long as a responsible person is present in the room, making sure
  that people do not switch stations.)  You can also require logins
  without having a password file: and login students on the machine
  using the master password which will work with any username.  (This is
  also useful if some students forgot their password.)

  You can even do so remotely from the server prompt using the `login`
  command, but note that if the server is restarted, the clients will
  need to be re-logged-in.  (This should be automated by making the
  server remember such forced logins, and replay them if it restarts.)

* If the server is set up to require logins, the client is still
  functional before such login is successful --- but it will not show
  editable files.  This is useful, for example, if you have twice the
  number of machines than the number of students: each student can then
  use one machine to write their answers, and another machine to read
  the exam material.

* Clients are identified with a unique client id (which you setup when
  the clients are started, or leave the default as the IP number), and
  after logging in, they are identified by usernames.  These must be
  unique, so no user can login on two clients at the same time (but if a
  machine crashes, it is possible to have the student move to a
  different machine).  In our lab, I set each client id to correspond to
  the physical location of the machine, so it is later possible to know
  if two students were sitting next to each other.  (When there's
  suspected copying, for example.)

* If a client is disconnected, and later reconnected, it will show the
  same content, including files that were modified by the logged-in
  username (if logins are required) or files that were modified on the
  client with the same client id --- so no edits (beyond the few seconds
  of poll frequency) are lost on a client crash.

* The server controller can send messages to all clients.  The message
  will flash at the bottom of each client to avoid a student missing it.
  The message is also logged in a "messages" buffer, so students can
  read these messages later again.  The controller can also send a
  message to a single client.

* Clients can send a message to the server, which is done with a
  chat-like interface.  Together with the above, it is possible for
  students to ask you questions privately, and for you to announce a
  message back for the whole class.  (For example, a student asks a
  question, you realize that further clarification is important so you
  edit a file or add a new one, and tell everyone about it.)  It is also
  possible to disable this feature in the configuration file.

* The server controller has extensive control over the clients through
  several commands (more below).  It is possible to freeze all clients
  (in case you want to make sure that students listen to what you're
  saying) or a specific one; force all clients (or a specific one) to
  switch to the messages window in case you want to make sure they're
  reading some messages that you're sending; unlock a client (or all)
  remotely; restart a client (or all) to recycle the machine for someone
  else; or kill a client (or all) which is useful at the end of the
  test.

* The clients can show the time --- which you can configure to be a
  normal clock, or a countdown to a specific time.  You can also change
  this time at the server prompt, including to any random (short) text
  (for example `1min!`).  In all cases, the displayed time is controlled
  by the server, so a station with a bad internal clock will not cause
  problems.

* Many more aspects of both the server and the clients are customizable.
  All the way down to indicator colors.


## Running the Server and the Clients

* Create a new working directory for your test, in this directory create
  the following (most of these can be configured to different names):

  - `config.rktd` file for customizations specific to your exam.  You
    can just copy the `config.rktd` file in the source directory to your
    working directory and change what you need.  You can also remove
    things that you don't need to change --- the configuration file in
    the source directory is used for defaults.  Some of the entries here
    are important --- more on this below.

  - A `content` directory with all the content that your student should
    see.  Most importantly, it should include some `messages` file whose
    contents will be displayed to the students when the test client
    starts up (in the configuration file it is marked by a path with a
    `mode` of `interaction`), and a text file with your exam text.  It
    should also include a file for the students to write their answers
    in --- this file can be blank, or it can have some template text in.
    You can of course have multiple files for the exam and for the
    answers (eg, one file for each question and one for each answer).
    Additional materials can be provided --- lecture notes, code files
    etc.  Use subdirectories to organize this content, since the clients
    will show that as a hierarchical list.  The `config.rktd` file has
    an entry to specify which files are editable, which files are
    presented in Racket mode (with syntax-based highlighting), the order
    that files are listed, and more.  Also note that this content can be
    updated while the exam is running: you can edit existing files (eg,
    when you find a typo), or add new files (like adding notes, hints,
    and examples).

  - A `passwords.rktd` file if you want students to log-in to be able to
    do the exam.  In the configuration file you can choose to let
    students work on any machine, and in that case you don't need this
    file.  (More below.)

    The supported format for this is currently a big list holding
    two-item lists, each holding a login name and the md5 checksum
    string of the password.  (It should be easy to extend this to other
    kinds of password hashes, like the one used in unix labs.  Mail me
    if you need such an extension.)

  - `private-key.pem` and `private-key.pem` to be used by the server.
    (See the handin-server for instructions on how to make those, or if
    you are using the handin server, you can just use the same files.)

* If you haven't done so, go over your local configuration file and
  change the entries that are specific to you.  Most importantly, you'll
  need to specify the machine that the server is running on, and the
  port number.

* While you're in this directory, run

      .../server.rkt

  which will start up the server.  The script will run the server code
  in a loop, so if it crashes for some reason it will start back up.
  (The clients are fairly robust, and should let students continue their
  work in this case, trying to reconnect to the server in the
  background.)  If you don't want to be running the server in a loop, or
  if you're not running it in a unix-like environment, you can just run
  racket directly with the file:

      racket .../server.rkt

  This will be more convenient to do in Emacs, since you then get its
  line-editing capabilities, and you can also set it to show log output
  directly (possibly in addition to a log file) using the `stderr` entry
  in the configuration file (Emacs helps here since log output that is
  displayed while you're entering a line will not mess up your input
  line).

* If you prefer to have a separate logfile and no stderr output, then
  you should open a new terminal window, and in it run something like

      tail -f LOG

  so you can see log entries as they come in.  (This include alerts in
  case a student is trying to hack around the client application ---
  exit from it and/or run a different application.)

* The server is now listening for connections, and the next step is to
  start up the clients.  The server makes this more convenient in two
  ways.  First, you don't need to install anything on the client
  machines beyond Racket --- and then copy a single `netboot.rkt` file
  which will retrieve the client sources from the server and run the
  client.  (This is also a safety feature: if there is a bug in the
  client, or a problem in the configuration file, running `netboot.rkt`
  will get the updated sources and configuration.)

  Secondly, since this `netboot.rkt` needs to connect to the server, it
  needs to know its address.  Instead of creating a "netboot.rkt" file
  that requires configuration, the server can make such a file for you.
  Enter `make-netboot` at the server prompt, and the file will be
  created (in the working directory).  The file can be a Racket file to
  be run with `racket`, or a batch file that will extract and run the
  Racket file.  Creating a batch file is more convenient because it
  leaves you with less actions to perform on each machine (this can be
  important if you have a lab of 50 machines --- you won't want to leave
  a machine without the client running, since that can be used by
  students).  See the configuration section for more details on how to
  configure the kind of netboot file that is generated.

* After you copy `netboot.rkt` to the client, you can run it with either
  the `Racket` executable.  It will create a new directory, contact the
  server and retrieve the client source files, then run the client.

* From this point on, clients are active and can be used to read the
  material and write answers (after logging in, if required by the
  configuration).  The server prompt has many commands that can be used
  to control clients, communicate with students, and much more.  (See
  below.)

* At the end of the exam, you can enter `kill -all` on the server prompt
  to terminate all clients.


## Configuration Options

The configuration file is basically a single S-expression with a
hierarchical structure for setting various options.  There are a few
global options, and then some options in sections for the server, the
client, and the netboot.  In the following list, entries that are marked
with "(!)" are ones that you will probably want to modify in your own
configuration file, and entries marked with "(!!)" are ones that you
will almost certainly need to modify.

* Global options

  - `workdir`: this is the path to the working directory where the
    server will do its work (and where the exam-specific configuration
    is found, the content, student files, etc).  It can be an absolute
    path, or a path that is relative to where the process was started
    (eg, the default `.` will make it use the directory the server
    process was run from).

 - (!!) `server-name`, `server-port`: the name of the server machine,
   and the port number to listen at.

 - (!!) `client-port`: the client connects to the server via
   `server-port` unless this is specified as non-`#f`, in which case it
   will use it.  This can be useful in case the server is listening on a
   private port which is forwarded to from a public one.

 - (!!) `master-password`: the md5 sum of the master password.  This
   password can be used to unlock a client (hit ctrl+shift+P, then the
   password and then enter), and it can also be used to log in any
   username (even ones that don't exist in the password file).  To set a
   master password, evaluate this code:

       (require file/md5)
       (md5 #"my-password")

   (Be careful with this, for example, if you run it in the DrRacket
   REPL then your password will be saved in the expression history.)

 - (!!) `path-specs`: a specification of the directory layout.  This is
   an important entry so it is described separately below.

* Server options

  - `poll-frequency`: the server periodically polls all clients.  This
    is done for security (to make sure that no client is killed), and
    more importantly to retrieve client edits for local storage.  This
    sets the time in seconds between these polls.

  - `read-timeout`: when the server is reading from a client, it will
    wait this many seconds before it decides that the client is dead.

  - `read-limit`: reading a message from the client is limited to this
    number of kilobytes.  (This is a safety measure to avoid malicious
    clients from crashing the server.)

  - (!) `accept-ips`: this can be set to a string which specifies an IP
    number prefix that will be accepted by the server.  For example,
    "12.34.56" means that any IP address that begins with these three
    numbers (followed by any number) will be accepted and any other IP
    address will be rejected.  The default, `#f`, means that all IP
    addresses are accepted.  (You will probably want to set this to some
    prefix that is specific to your lab's network.)  It can also be a
    list of such strings.

  - (!) logfile, stderr, prompt: the log file that the server will use
    for all alerts (if any), whether it should show log messages on
    stderr, and the prompt that the server shows for the controller
    (you).  As said above, if you're using a log file and no stderr, you
    will need to run `tail -f logfile` in a separate terminal window so
    you know about important alerts.  Another alternative is to set
    `prompt` to `#f` to have no prompt: in this case the stderr output
    will be mixed with your input --- but if you run this in Emacs it
    will keep your input line intact so you get a convenient interface.

  - (!) `login-mode`: this is a boolean flag.  When it is #t (the
    default), students need to log in to be able to work.  This does not
    require a password file: you can login students from the server
    (using any username that is convenient, like the student's name).
    When this flag is set to `#f`, clients are immediately ready to edit
    answer files.  In this case you will need to instruct them to write
    their name in the file, and you will need to keep an eye so that
    students don't switch machines.  (In my experience, this is much
    more practical than it sounds --- in fact, this security model is
    similar to each student having their own piece of paper, but a
    little better since changing seats is more difficult than changing
    papers.)

  - (!) `password-verifier`: if you choose to have students login using
    known username/password pairs, you need to setup a file with this
    information and set this to be a list that has the hash method and
    the password file (which can be an absolute path or relative to the
    working directory).  Currently, only md5 hashes are supported (which
    is what the handin server uses), with a file that is formatted as a
    list of two-element lists, each holding a username and an md5 hash.
    [It is easy to add other methods if needed --- it will require a new
    `passwords-*.rkt` source file to implement.]

  - (!) `content-dir`: the directory where all exam content is found.
    This includes read-only files for reading, a messages file for the
    greeting message, and editable file(s) for writing answers.  The
    directory hierarchy of this is mirrored in the client interface.
    Use the `path-specs` configuration option (see below) to describe
    the various files.

  - (!) `clients-dir`: the directory where files that clients edit is
    stored at.  Each client will have a separate directory, named after
    the username if login-mode is on, or after the client id otherwise.
    Also, this name will be used for backup directories (eg, if this is
    set to `clients`, then backup directories will be called
    `clients-YYYYMMDDHHMMSS`).

  - (!) `allow-messages?`: if this is `#t`, then clients are allowed to
    send messages to the server controller (to ask questions), otherwise
    this is disabled.  Note there is a server command to disable/enable
    this dynamically if needed.

  - `backup-interval`: perform a backup of the clients directory every
    this many seconds.  Backups are done only when polling, the the most
    you can get from this (eg, set to `0`) is one backup every poll.
    You can also set it to `#f` to disable backups (not a good idea!).
    Each backup is a directory copy of the clients-dir (set above),
    named by a suffix that indicates the date and time of the backup.

  - backup-command: if this is set to `#f`, then `copy-directory/files`
    will be used to perform a backup.  Otherwise, you can set it to a
    list holding a command name and flags to use for the backup --- the
    default is `("cp" "-al")` which uses the linux `cp` command in a
    mode that creates hard-links instead of copies, so frequent backups
    do not take much time or disk space.  (Note that when a client sends
    an edit, the file is *replaced* so backups are not modified too.)

* Netboot options

  - (!) `file`: the netboot file name that the server will create for
    you when you use the `make-netboot` command.  The suffix of this
    file determines the kind of netboot that is generated --- either a
    Racket file or a batch file (which will extract and run the Racket
    netboot file, possibly in a loop).

  - `client-dir`: when the netboot script runs, it will contact the
    server to get the client source files, and put them in this
    directory (creating it, and asking permission to delete it if it
    exists).  This can be a relative path (relative to where the netboot
    process is run from) or an absolute one.

  - `client-files`: lists the files that are needed to run the client.
    The first file is what gets executed to run the client.

  - (!) `racket-path`: the path to the Racket directory to use in a
    batch file netboot.  (Absolute or relative to where it is running
    from.)  Can also be a list of such paths --- the first one that
    exists will be used.

  - (!) `batch-prefix`: a list of batch lines to put at the beginning of
    a batch netboot file.  For example, you can use `("C:" "cd \\Temp")`
    to switch to `C:\Temp` before extracting and running the Racket
    netboot script.

  - (!) `batch-loop?`: if this is true (the default), then a netboot
    batch file will run the Racket script in a loop, waiting for a
    keypress between runs.  This can be convenient in a lab when many
    clients need to be restarted.

* Client options

  - (!!) `app-locked?`: controls whether clients lock the machine in
    "kiosk mode".  The default is set to `#f` to avoid accidentally
    locking your machine (did you set a master password yet?).  Set it
    to `#t` when you get to run the clients in the lab.

  - `read-timeout`: similar to the `read-timeout` entry in the server
    section, only this determines when the client decides that the
    server is not responding.  This should be relatively larger than the
    poll frequency, since the client uses this when it is waiting for
    server polls.

  - (!) `ask-for-client-id?`: if this is set to `#t`, the client will
    ask for a client-id to use when it starts (the idea is that you go
    around the lab and assign client ids when you start things up).  A
    client id will be saved (in a local file) and used as the default
    value if the client runs again.  You can also set it to `#f`, which
    will make it use such a saved or otherwise the client will have its
    IP address as an id.  Finally, `if-missing` (the default) will ask
    for an id only if no saved id is found (ie, the first time you run a
    client).

  - (!) `id-file-directories`: directories to try to use when saving and
    reading the client-id.  You should make sure that this directory
    exists on the lab machines, and that it is local.  (For example,
    don't use your home directory if you need to log onto all of these
    machines.)

  - (!) `show-clock?`: should clients show a clock on the bottom of the
    screen?  If this is set to #t, then clients will show it --- the
    time is updated from the server so it does not depend on client
    internal clocks.  Also, you can use the `set-time` command on the
    server prompt to change the time that is displayed (see below).

  - `buffer-limit`: limit on the size of buffers, measured in kilobytes.
    It is best to make this considerably smaller than the `read-limit`
    server option, to avoid clients disconnecting due to buffers that
    are too long.

  Note: the following options are GUI customization, see the source
  configuration file for a syntax for the different kinds of options.
  (When the options below are `font`, `style`, or `color`, then this
  means a description of such a feature is expected.)

  - `default-font-size`, `min-font-size`, `max-font-size`: default size
    for displayed content, and maximum and minimum sizes it can change
    to (the size is configurable on the client).  Be careful not to
    allow sizes that are too big so students can easily read nearby
    screens.

  - `message-font`, `clock-font`: the font used to display status-bar
    messages, and the clock.

  - `toc-item-style`: the default style to use for the right-hand-side
    hierarchical list of files and directories.

  - `status-color`, `alert-color`, `message-color`: the color that is
    used in the messages window for status messages, alert messages, and
    messages from the server (when you enter some text).

  - `unsynced-color`, `unlocal-editable-color`, `unseen/new-color`,
    `unread-messages-color`: colors are used to indicate the state of
    different "buffers" on the right-hand-side list.  The first
    indicates an editable buffer that was not yet synchronized to the
    server; the second is for an editable buffer that the student did
    not modify yet (ie, there is no client-local version on the server,
    and no edits done on the client); the third indicates a text that
    the student didn't see yet (didn't switch to), including files that
    were created while the clients are running (eg, clarifications,
    typos, and hints); the fourth color highlights the messages buffer
    when the server sent a messages (which is logged there).

* (!!) Path specs

  This entry is used by both the server and the client to set properties
  for paths in your content tree.  Its value is a list of path
  specification.  The provided configuration file has a sample spec,
  which you will need to modify for your exam's content (it also has a
  more formal description of the syntax).

  Each specification entry looks like

      [<path> <pathdata> ...]

  where the `<path>` specifies paths that this entry applies for, and
  the rest specify properties for it.  When a path property is searched,
  each of these entries is tried in order, returning the property value
  when it is first found.  The `<path>` part is a list of values that
  correspond to the path elements, each can be a string (a file or a
  directory name), a regexp (all file/directory names that match), the
  symbol `?` (any name), or the symbol `*` (any number of path elements
  with any name).  `(*)` can therefore be used as a default entry.  Note
  that each property is searched separately, so you can have, for
  example, several `(*)` entries for different properties.  The
  properties themselves are specified as an even-length list of property
  name and a value.

  These are the properties that are used:

  - `ignore`: determines whether the path is ignored (which means that the
    clients never receive it).  For example,

        [(* #rx"^[.]" *) ignore #t]

    makes the server ignore all directories and filenames that begin
    with a period.

  - `editable`: determines whether this is a readonly file or an
    editable one (ie, used for writing answers).  You can have more than
    one editable file --- for example, you can have a separate file for
    each question.

  - `mode`: determines the mode of files.  Can be `text` or `racket` for
    text and files (racket mode makes a buffer similar to DrRacket's
    definitions window).

    It can also be `interaction` for the messages file: this is a
    special mode that makes the corresponding file's contents appear in
    the greeting message on the client, and further notifications
    (status messages, alerts, and server messages) will be kept in this
    buffer --- you should have at most one file with this mode.  If
    don't set one, then the messages window will not be selectable on
    the clients (but the `tell!` command on the server will still switch
    to it).

  - `order`: the paths are usually ordered in lexicographic order (but
    number parts in names are sorted numerically); this property can be
    used to force a different order (that is, entries are sorted
    according to the `order` properties and then by their name).  For
    example,

        [("exam.txt") order -10]

    will place the exam text before other entries.  You can also split
    your exam to several files for each question and one for each
    answer, and use the order property to make each pair appear
    together.

  - `comparator`: this is an alternative way for sorting paths --- it
    can name a comparison function that is different from the
    lexicographic comparison.  If the same comparator is specified for
    two names in the same directory, it will be used to sort them,
    otherwise they are sorted as usual (by an `order` specification and
    then lexicographically).  The function can return `'?` to specify
    that the default ordering should be used.  Currently, this is
    specified as a symbolic name from a fixed set --- and the only
    possible value is `qa-comparator`, which compares names so
    `Question<N>*` is grouped with `Answer<N>*`, and a `Scratch.*` is at
    the end (this is a convenient configuration for separate
    question/answer files for different exam questions).

  - `style`: the style of the hierarchical list item for these paths.
    You can use this to highlight some files (eg, the exam text).

  - `keep-1st-line`: a regular expression that determines when the first
    line in a file should always be shown (similar to the way that
    DrRacket makes the `#lang` line sticky).


## Server Commands

When the server starts, it provides you with a prompt where you can
enter various commands.  Some of these commands expect a client id
(marked as `<id>`) --- for these, you can use the client id (or the
logged in username), or `-all` to make the command apply to all clients.
Also, `^` means "the last value" and can be used in several places: as
the command (use the last command), as the `<id>` (apply a command to
the last named client identifier), and as the `text...` part of a `tell`
command (use the same text).

* `help` --- show a brief summary of the available commands.

* `list` --- list the connected clients, with their IP addresses, client
  ids, and usernames (when a client is logged on).

* `;...` --- any line that begins with a semicolon is shown in the
  logfile.  This is useful to add entries that mark certain events that
  you want to record.

* `tell <id> text...` --- show a message on a client specified by
  `<id>`.

* `tell! <id> text...` --- similar to `tell`, but also makes clients
  switch to the messages window.  (Useful when you want to make sure
  that everyone sees your message.)

* `messages <id> {on|off}` --- enables or disables a client from sending
  messages.

* `set-time time...` --- sets the time that is displayed by clients.
  This can be the current time, a countdown to a specific time, or a
  fixed string (up to 6 characters).  Use `set-time help` to see the
  syntax for these options.

* `freeze <id> {on|off}` --- freeze (or unfreeze) client(s), making them
  ignore all input events.  (For example, you can use `freeze -all on`
  to force a break.)

* `login <id> <username>` --- make a client log in as a username.  The
  username can be anything you want, but it cannot have spaces.  Cannot
  be used if the client is already logged in (you can use `restart`
  first to have a different login).

* `unlock <id>` --- unlock the client application, making it possible to
  switch to other windows, or close it.

* `restart <id>` --- restart the client (the GUI will shut down and
  restart).  This can be useful if you want to "recycle" a machine for a
  different student to log into.  (Note that the client is still the
  same process, so if, for example, you change the `app-locked?`
  setting, the restarted client will not be affected.)

* `kill <id>` --- terminate the client.  At the end of the exam, you can
  just enter `kill -all` to shut down all clients.

* `make-netboot` --- create a network boot file to run remotely on
  client machines.  (See above.)
