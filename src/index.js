import { Main } from './Main.elm'
// import InitJS from './App/JS'
import { loadPlayer } from './JS/YTPlayer'
import registerServiceWorker from './registerServiceWorker'

var app = Main.embed(document.getElementById('root'))

app.ports.elmData.subscribe(msg => {
  switch (msg.tag) {
    case 'LoadYouTubeVideo':
      console.log('LoadYouTubeVideo', msg.data)
      // loadPlayer(msg.data)
      break

    default:
      break
  }
})

app.ports.jsData.send({ tag: 'JSPlayerStatus', data: null })

// InitJS()

registerServiceWorker()
