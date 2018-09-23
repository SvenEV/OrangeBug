namespace OrangeBug.DesktopClient

open OrangeBug

type 'a Animation = {
    source: 'a
    target: 'a
    startTime: SimTime
    duration: SimTimeSpan
}

module Ease =
    let linear t = t
    let quadraticIn t = t * t
    let quadraticOut t = -t * (t - 2)
    let quadraticInOut t =
        if t < 0.5f
        then 0.5f * t * t
        else let t = t - 1.0f in -0.5f * (t * (t - 2.0f) - 1.0f)
    let sineIn t = 1.0f - (cos (t * pi'2))
    let sineOut t = sin (t * pi'2)
    let sineInOut t = -0.5f * (cos (t * pi) - 1.0f)

module Animation =
    let create source target startTime duration =
        { source = source; target = target; startTime = startTime; duration = duration }

    let constant value = create value value (SimTime 0) (SimTimeSpan 0)

    let evaluate interpolate (ease: float32 -> float32) simTime animation =
        let elapsed = simTime - float32 animation.startTime.value
        let progress = clamp01 (elapsed / float32 animation.duration.value) |> ifNaN 0.0f |> ease
        interpolate animation.source animation.target progress

    let evaluateFloat = evaluate lerp
    let evaluateVector2 = evaluate (fun a b t -> XnaVector2.Lerp(a, b, t))
    let evaluateVector3 = evaluate (fun a b t -> XnaVector3.Lerp(a, b, t))
    let evaluateVector4 = evaluate (fun a b t -> XnaVector4.Lerp(a, b, t))
    let evaluateMatrix = evaluate (fun a b t -> XnaMatrix.Lerp(a, b, t))