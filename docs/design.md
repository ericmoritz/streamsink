# Servers

## stream_fsm

### monitoring

In the monitoring phase, the stream_fsm periodically connects to the
stream.  It will monitor the stream until it gets the stream's
meta data.  If the meta data matches a show scheduled for recording,
the FSM will switch to the `recording` phase, otherwise it disconnects
the stream and schedules a timeout for the next periodic check of the
stream.  The monitoring phase exists to prevent unneeded bandwidth
usage ignoring the stream when not recording it.

### recording

When switching to the recording phase, a new `stream_recorder` process
is started and stream_fsm awaits until the `stream_recorder` is ready.

Once the `stream_recorder` is ready, the stream_fsm forwards incoming
mp3 packets to the `stream_recorder` process.

If the show name changes while in the recording phase, the new name is
an "end" message is sent to the `stream_recorder` process which causes
it to consume the buffered mp3 packets and close its file.

If the new show is a scheduled recording, a new `stream_recorder`
process is started, awaited, and the recording phase continues with
the new `stream_recorder` process, otherwise, the FSM returns to the
`monitoring` phase.


### stream_recorder

This records a stream to a file.  It is the error kernel for a
stream.  It is started with a proplist of metadata which is used to
create an mp3 file.  It receives two types of messages {mp3, binary()}
| end.  

# Supervision

The `stream_fsm` and the `stream_recorder` are both registered using
`gproc` and are looked up using logical names rather than `pids()`.

Processes of both type are `transient` which enables them to be
restarted if they die abnormally or fade away if they die normally.









