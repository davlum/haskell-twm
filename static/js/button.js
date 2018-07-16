function __log(e, data) {
  log.innerHTML += "\n" + e + " " + (data || "")
}

var audio_context
var recorder

function startUserMedia(stream) {
  var input = audio_context.createMediaStreamSource(stream)

  recorder = new Recorder(input)
}

function startRecording(button) {
  recorder && recorder.record()
  button.disabled = true
  button.nextElementSibling.disabled = false
  __log("Recording...")
}

function stopRecording(button) {
  recorder && recorder.stop()
  button.disabled = true
  button.previousElementSibling.disabled = false
  __log("Stopped recording.")

  // create WAV download link using audio data blob
  createDownloadLink()

  recorder.clear()
}

function createDownloadLink() {
  recorder &&
    recorder.exportWAV(function (blob) {
      var formData = new FormData()
      if (blob) {
        var recording = new Blob([blob], { type: "audio/wav" })
        formData.append("recording", recording)
      }

      let url = URL.createObjectURL(blob)
      let li = document.createElement("li")
      let au = document.createElement("audio")
      let hf = document.createElement("a")
      let br = document.createElement("br")

      au.controls = true
      au.src = url
      hf.href = url
      hf.download = new Date().toISOString() + ".wav"
      hf.innerHTML = hf.download
      li.appendChild(au)
      li.appendChild(br)
      li.appendChild(hf)
      document.getElementById("recordingslist").appendChild(li)
      $.post({
        processData: false,
        contentType: false,
        enctype: "multipart/form-data",
        url: "/upload",
        data: formData,
      }, (data) => {
        alert("The most common note is " + data.note);
      })
    })
}

window.onload = function init() {
  try {
    // webkit shim
    window.AudioContext = window.AudioContext || window.webkitAudioContext
    navigator.getUserMedia =
      navigator.getUserMedia ||
      navigator.webkitGetUserMedia ||
      navigator.mozGetUserMedia ||
      navigator.msGetUserMedia
    window.URL = window.URL || window.webkitURL

    audio_context = new AudioContext()
    __log("Audio context set up.")
    __log(
      "navigator.getUserMedia " +
      (navigator.getUserMedia ? "available." : "not present!"),
    )
  } catch (e) {
    alert("No web audio support in this browser!")
  }
  navigator.getUserMedia({ audio: true }, startUserMedia, function (e) {
    __log("No live audio input: " + e)
  })
}
