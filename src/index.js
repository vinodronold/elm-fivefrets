import { Main } from './Main.elm'
import registerServiceWorker from './registerServiceWorker'

var jsPlayer, jsPlayerID, jsPlayerVideoID, jsPlayerListerner
var app = Main.embed(document.getElementById('root'))

app.ports.elmData.subscribe(({ tag, data }) => {
  switch (tag) {
    case 'LoadYouTubeVideo':
      console.log('LoadYouTubeVideo', data)
      var tag = document.createElement('script')
      tag.src = 'https://www.youtube.com/iframe_api'
      var firstScriptTag = document.getElementsByTagName('script')[0]
      firstScriptTag.parentNode.insertBefore(tag, firstScriptTag)
      window.onYouTubeIframeAPIReady = getYouTubeIframeAPIReady(
        data.playerID,
        data.youTubeID
      )

      break

    case 'PlayVideo':
      jsPlayer.playVideo()
      break

    case 'PauseVideo':
      jsPlayer.pauseVideo()
      break

    case 'StopVideo':
      jsPlayer.stopVideo()
      break

    case 'GetPlayerCurrTime':
      app.ports.jsData.send({
        tag: 'JSPlayerCurrTime',
        data: jsPlayer.getCurrentTime()
      })
      break

    case 'SeekTo':
      console.log('SeekTo -> ', data)
      if (jsPlayer) {
        jsPlayer.seekTo(data, true)
      }
      break

    default:
      break
  }
})

registerServiceWorker()

const getYouTubeIframeAPIReady = (jsPlayerID, jsPlayerVideoID) => () => {
  jsPlayer = new YT.Player(jsPlayerID, {
    height: 'auto',
    width: 'auto',
    videoId: jsPlayerVideoID,
    playerVars: {
      autoplay: 1,
      controls: 0,
      fs: 0,
      iv_load_policy: 3,
      modestbranding: 1,
      playsinline: 1,
      rel: 0,
      showinfo: 0
    },
    events: {
      onStateChange: onPlayerStateChange
    }
  })
}

const onPlayerStateChange = e => {
  console.log('State: ' + ' (' + e.data + ').')
  app.ports.jsData.send({ tag: 'JSPlayerStatus', data: e.data })
}
