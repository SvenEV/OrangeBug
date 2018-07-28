import { TextureLoader, Texture, Audio, AudioLoader, AudioListener, AudioBuffer } from "three";

export class AssetLoader {

    private static readonly textureLoader = new TextureLoader()
    private static readonly audioLoader = new AudioLoader()
    private static readonly loadedSprites: { [key: string]: Texture } = {}
    private static readonly loadedSounds: { [key: string]: Audio } = {}
    private static audioListener: AudioListener

    static initialize(audioListener: AudioListener) {
        this.audioListener = audioListener
    }

    static getSprite(key: string) {
        let sprite = this.loadedSprites[key]

        if (sprite)
            return sprite

        let texture = this.textureLoader.load("images/sprites/" + key + ".png")
        this.loadedSprites[key] = texture
        return texture
    }

    static getSoundAsync(key: string): Promise<Audio> {
        return new Promise((resolve, reject) => {
            let sound = this.loadedSounds[key]

            if (sound)
                resolve(sound)

            this.audioLoader.load("sounds/" + key + ".mp3", (buffer: AudioBuffer) => {
                let audio = new Audio(this.audioListener)
                audio.setBuffer(buffer)
                this.loadedSounds[key] = audio;
                resolve(audio)
            }, null, null)
        })
    }
}