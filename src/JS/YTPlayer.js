import YouTubePlayer from 'youtube-player'

const getPlayer = () => YouTubePlayer('YT_Player')

const loadPlayer = ytid => {
  getPlayer().loadVideoById(ytid)
}

export { loadPlayer }
