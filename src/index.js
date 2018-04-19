import { Main } from './Main.elm'
import YouTubePlayer from 'youtube-player'
import registerServiceWorker from './registerServiceWorker'

var jsPlayer, jsPlayerListerner
var app = Main.embed(document.getElementById('root'))

app.ports.elmData.subscribe(({ tag, data }) => {
  switch (tag) {
    case 'LoadYouTubeVideo':
      console.log('LoadYouTubeVideo', data)
      jsPlayer = YouTubePlayer(data.playerID, {
        videoId: data.youTubeID,
        height: 'auto',
        width: 'auto',
        playerVars: {
          autoplay: 1,
          controls: 0,
          fs: 0,
          iv_load_policy: 3,
          modestbranding: 1,
          playsinline: 1,
          rel: 0,
          showinfo: 0
        }
      })

      jsPlayerListerner = jsPlayer.on('stateChange', e => {
        console.log('State: ' + ' (' + e.data + ').')
        app.ports.jsData.send({ tag: 'JSPlayerStatus', data: e.data })
      })
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
      jsPlayer.getCurrentTime().then(currTime => {
        console.log(currTime)
        app.ports.jsData.send({ tag: 'JSPlayerCurrTime', data: currTime })
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
