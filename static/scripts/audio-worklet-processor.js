class AudioProcessor extends AudioWorkletProcessor {
    static get parameterDescriptors() {
        return [];
    }

    constructor() {
        super();
    }

    process(inputs, outputs) {
        const inputBuffer = inputs[0][0];
        const outputBuffer = outputs[0][0];
        
        if (!inputBuffer || !outputBuffer) {
            return false;
        }
        
        const inputChannel = inputBuffer.getChannelData(0);
        const outputChannel = outputBuffer.getChannelData(0);
        
        for (let i = 0; i < outputChannel.length; i++) {
            outputChannel[i] = inputChannel[i];
        }
        
        return true;
    }
}

registerProcessor('audio-processor', AudioProcessor);

