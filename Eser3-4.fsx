#load "lwc.fsx"

#nowarn "0058"

open System.Windows.Forms
open System.Drawing

open Lwc

let transformPoint (m:Drawing2D.Matrix) (p:PointF) =
    let pts = [| p |]
    m.TransformPoints(pts)
    pts.[0]

type FontList() =    
    static member Array = ResizeArray<Font>([| 
        Font("Arial", 20.f)
        Font("Verdana", 20.f)
        Font("Papirus", 20.f)
        Font("Vivaldi", 15.f)
    |])

    static member First =
        FontList.Array.[0]
    static member Next f =
        let i = FontList.Array.IndexOf(f)
        FontList.Array.[(i+1) % FontList.Array.Count]
        
type LWTControl() as this =
    inherit LWControl()

    let mutable transform = W2V()

    member this.Transform 
        with get() = transform
        and set(v) = transform <- v

    
    override this.HitTest p =
        let pw = [| p |]
        transform.V2W.TransformPoints(pw)
        pw.[0]
        RectangleF(this.Position, this.Size).Contains(pw.[0])

type KeyCont() as this =
    inherit LWContainer() 

    let animationTimer = new System.Timers.Timer(1000. / 60.)
    
    let mutable letter = "D"
    let mutable selectedControl: LWTControl option = None
    let mutable isMoving = false
    let mutable animationState = 0

    let rotateAnimation = fun _ ->
        this.LWControls
        |> Seq.filter (fun c -> c.CoordinateType = World)
        |> Seq.iter (fun c ->
            if(selectedControl.IsNone || (selectedControl.Value <> (c:?>LWTControl))) then
                let c = (c :?> LWTControl)
                let centerX = c.Size.Width / 2.f
                let centerY = c.Size.Height / 2.f
                c.Transform.Translate(c.Position.X, c.Position.Y)
                c.Transform.Translate(centerX, centerY)
                c.Transform.Rotate(5.f)
                c.Transform.Translate(-centerX, -centerY)
                c.Transform.Translate(-c.Position.X, -c.Position.Y)
        )
        this.Invalidate()

    let allInAnimation = fun _ ->
        let mutable zoom = false
        this.LWControls
        |> Seq.filter (fun c -> c.CoordinateType = World)
        |> Seq.iter (fun c ->
            if(selectedControl.IsNone || (selectedControl.Value <> (c:?>LWTControl))) then
                let c = (c :?> LWTControl)
                let v1 = transformPoint this.Transform.W2V (transformPoint c.Transform.W2V (c.Position))
                let v2 = transformPoint this.Transform.W2V (transformPoint c.Transform.W2V (PointF(c.Position.X + c.Size.Width, c.Position.Y)))
                let v3 = transformPoint this.Transform.W2V (transformPoint c.Transform.W2V (PointF(c.Position.X, c.Position.Y + c.Size.Height)))
                let v4 = transformPoint this.Transform.W2V (transformPoint c.Transform.W2V (PointF(c.Position.X + c.Size.Width, c.Position.Y + c.Size.Height)))

                if  v1.X < 0.f || v1.Y < 0.f || v1.X > single this.Size.Width || v1.Y > single this.Size.Height  || 
                    v2.X < 0.f || v2.Y < 0.f || v2.X > single this.Size.Width || v2.Y > single this.Size.Height  || 
                    v3.X < 0.f || v3.Y < 0.f || v3.X > single this.Size.Width || v3.Y > single this.Size.Height  || 
                    v4.X < 0.f || v4.Y < 0.f || v4.X > single this.Size.Width || v4.Y > single this.Size.Height  then
                        zoom <- true
        )
        if zoom then
            let centerX = single this.Size.Width / 2.f
            let centerY = single this.Size.Height / 2.f
            let newCenter = transformPoint this.Transform.V2W (PointF(centerX, centerY))
            this.Transform.Translate(newCenter.X, newCenter.Y)
            this.Transform.Scale(1.f/1.1f, 1.f/1.1f)
            this.Transform.Translate(-newCenter.X, -newCenter.Y)
            rotateAnimation(true)
            
            this.Invalidate()

        else
            this.LWControls
            |> Seq.filter (fun c -> c.CoordinateType = World)
            |> Seq.iter (fun c ->
                if(selectedControl.IsNone || (selectedControl.Value <> (c:?>LWTControl))) then
                    let c = (c :?> LetterSquareAnimate)

                    c.Transform.Translate(c.Velocity.X, c.Velocity.Y)

                    let v1 = transformPoint this.Transform.W2V (transformPoint c.Transform.W2V (c.Position))
                    let v2 = transformPoint this.Transform.W2V (transformPoint c.Transform.W2V (PointF(c.Position.X + c.Size.Width, c.Position.Y)))
                    let v3 = transformPoint this.Transform.W2V (transformPoint c.Transform.W2V (PointF(c.Position.X, c.Position.Y + c.Size.Height)))
                    let v4 = transformPoint this.Transform.W2V (transformPoint c.Transform.W2V (PointF(c.Position.X + c.Size.Width, c.Position.Y + c.Size.Height)))

                    if  v1.X < 0.f || v1.Y < 0.f || v1.X > single this.Size.Width || v1.Y > single this.Size.Height  || 
                        v2.X < 0.f || v2.Y < 0.f || v2.X > single this.Size.Width || v2.Y > single this.Size.Height  || 
                        v3.X < 0.f || v3.Y < 0.f || v3.X > single this.Size.Width || v3.Y > single this.Size.Height  || 
                        v4.X < 0.f || v4.Y < 0.f || v4.X > single this.Size.Width || v4.Y > single this.Size.Height  then
                            c.Transform.Translate(-c.Velocity.X, -c.Velocity.Y)
                            c.Velocity <- PointF(-c.Velocity.X, c.Velocity.Y)
                            c.Transform.Translate(c.Velocity.X, c.Velocity.Y)
                    
                            let v1 = transformPoint this.Transform.W2V (transformPoint c.Transform.W2V (c.Position))
                            let v2 = transformPoint this.Transform.W2V (transformPoint c.Transform.W2V (PointF(c.Position.X + c.Size.Width, c.Position.Y)))
                            let v3 = transformPoint this.Transform.W2V (transformPoint c.Transform.W2V (PointF(c.Position.X, c.Position.Y + c.Size.Height)))
                            let v4 = transformPoint this.Transform.W2V (transformPoint c.Transform.W2V (PointF(c.Position.X + c.Size.Width, c.Position.Y + c.Size.Height)))

                            if  v1.X < 0.f || v1.Y < 0.f || v1.X > single this.Size.Width || v1.Y > single this.Size.Height  || 
                                v2.X < 0.f || v2.Y < 0.f || v2.X > single this.Size.Width || v2.Y > single this.Size.Height  || 
                                v3.X < 0.f || v3.Y < 0.f || v3.X > single this.Size.Width || v3.Y > single this.Size.Height  || 
                                v4.X < 0.f || v4.Y < 0.f || v4.X > single this.Size.Width || v4.Y > single this.Size.Height  then
                                c.Transform.Translate(-c.Velocity.X, -c.Velocity.Y)
                                c.Velocity <- PointF(-c.Velocity.X, -c.Velocity.Y)
                                c.Transform.Translate(c.Velocity.X, c.Velocity.Y)

                                
                                let v1 = transformPoint this.Transform.W2V (transformPoint c.Transform.W2V (c.Position))
                                let v2 = transformPoint this.Transform.W2V (transformPoint c.Transform.W2V (PointF(c.Position.X + c.Size.Width, c.Position.Y)))
                                let v3 = transformPoint this.Transform.W2V (transformPoint c.Transform.W2V (PointF(c.Position.X, c.Position.Y + c.Size.Height)))
                                let v4 = transformPoint this.Transform.W2V (transformPoint c.Transform.W2V (PointF(c.Position.X + c.Size.Width, c.Position.Y + c.Size.Height)))

                                if  v1.X < 0.f || v1.Y < 0.f || v1.X > single this.Size.Width || v1.Y > single this.Size.Height  || 
                                    v2.X < 0.f || v2.Y < 0.f || v2.X > single this.Size.Width || v2.Y > single this.Size.Height  || 
                                    v3.X < 0.f || v3.Y < 0.f || v3.X > single this.Size.Width || v3.Y > single this.Size.Height  || 
                                    v4.X < 0.f || v4.Y < 0.f || v4.X > single this.Size.Width || v4.Y > single this.Size.Height  then
                                    c.Transform.Translate(-c.Velocity.X, -c.Velocity.Y)
                                    c.Velocity <- PointF(-c.Velocity.X, -c.Velocity.Y)
                                    c.Transform.Translate(c.Velocity.X, c.Velocity.Y)           
                        
                    this.Invalidate()
            )
            

    do
        this.SetStyle(ControlStyles.AllPaintingInWmPaint ||| ControlStyles.OptimizedDoubleBuffer, true)
        animationTimer.Elapsed.Add(allInAnimation)

    member this.IsMoving 
        with get() = isMoving
        and set(v) = isMoving <- v
    member this.Letter
        with get() = letter
        and set(v) = letter <- v
    member this.SelectedControl
        with get() = selectedControl
        and set(v) = selectedControl <- v

    member this.ToggleAnimation =
        if animationTimer.Enabled then
            animationTimer.Stop()
        else
            animationState <- 0
            animationTimer.Start()


    override this.OnKeyDown e =
        if int e.KeyCode = 32 then
            this.ToggleAnimation
        else if int e.KeyCode >= 65 && int e.KeyCode <= 90 then
            letter <- string e.KeyCode
            this.Invalidate() 


    override this.OnResize _ =
        this.Invalidate()
        
    override this.OnPaint e =
        let g = e.Graphics
        g.SmoothingMode <- Drawing2D.SmoothingMode.HighQuality

        let t = g.Transform

        g.Transform <- this.Transform.W2V

        for idx in (this.LWControls.Count - 1) .. -1 .. 0 do
          let c = this.LWControls.[idx]:?> LWTControl
          if c.CoordinateType = World then
            let state = g.Save()//
            let m = g.Transform
            m.Multiply(c.Transform.W2V)
            g.Transform <- m
            c.OnPaint e
            g.Restore(state)//
        
        g.Transform <- t

        for idx in (this.LWControls.Count - 1) .. -1 .. 0 do
          let c = this.LWControls.[idx]
          if c.CoordinateType = View then
            c.OnPaint e
        

        let centerX = this.Size.Width / 2
        let centerY = this.Size.Height / 2
        g.DrawLine(Pens.Black, centerX - 10, centerY, centerX + 10, centerY)
        g.DrawLine(Pens.Black, centerX, centerY - 10, centerX, centerY + 10)
        
    
    override this.OnMouseDown e =
        let p = PointF(single e.X, single e.Y)
        let controlsView = this.LWControls |> Seq.filter (fun c -> c.CoordinateType = View)
        match (controlsView |> Seq.tryFind (fun c -> c.HitTest p)) with
        | Some c -> c.OnMouseDown(e)
        | None -> 
            let pw = transformPoint this.Transform.V2W p
            let controlsWorld = this.LWControls |> Seq.filter (fun c -> c.CoordinateType = World)
            match (controlsWorld |> Seq.tryFind(fun c -> c.HitTest pw)) with
                | Some c -> c.OnMouseDown(e)
                | None ->
                    match selectedControl with
                    | Some(v) -> 
                        (v:?>SelRectangle).Selected <- false
                        selectedControl <- None
                        this.Invalidate()
                    | None -> ()

    override this.OnMouseMove e =
        if isMoving then
            match selectedControl with
            | Some(c) ->
                let p: PointF = (c:?>SelRectangle).DragPoint
                let mv1 = transformPoint this.Transform.V2W (PointF(single e.X,single e.Y))
                let mv = transformPoint c.Transform.V2W mv1
                c.Transform.Translate(mv.X-p.X, mv.Y-p.Y)
                this.Invalidate()
            | None -> ()
        else
            base.OnMouseMove e

    override this.OnMouseUp e =
        if isMoving then
            isMoving <- false
        else
            base.OnMouseUp e

and LWRectangle() =
    inherit LWTControl()
    let mutable drawColor = Pens.Black
    let mutable fillColor = Brushes.Transparent
    member this.DrawColor 
        with get() = drawColor
        and set(v) = drawColor <- v
    member this.FillColor 
        with get() = fillColor
        and set(v) = fillColor <- v

    override this.OnPaint e = 
        base.OnPaint e
        let g = e.Graphics
        let r = RectangleF(this.Position, this.Size) |> RectF2Rect
        g.DrawRectangle(drawColor, r)
        // g.FillRectangle(fillColor, r)
        g.FillRectangle(fillColor, RectangleF(PointF(this.Position.X + 1.f, this.Position.Y + 1.f), SizeF(this.Size.Width - 1.f, this.Size.Height - 1.f)) |> RectF2Rect)

and LWRectangleNoHit() =
    inherit LWRectangle()

    override this.HitTest _ = false

and SelRectangle() as this =
    inherit LWRectangle()
    

    let mutable selected = false
    
    let mutable dragPoint: PointF = PointF(0.f, 0.f)

    member this.DragPoint 
        with get() = dragPoint
        and set(v) = dragPoint <- v
    member this.Selected
        with get() = selected
        and set(v) = selected <- v

    
    override this.OnMouseDown e =
        base.OnMouseDown e
        let sl = (this.Parent:?>KeyCont).SelectedControl
        match sl with
            | Some(value) ->
                (value:?>SelRectangle).Selected <- false
            | None -> ()
        (this.Parent:?>KeyCont).SelectedControl <- Some(this:>LWTControl)
        selected <- true
        (this.Parent:?>KeyCont).IsMoving <- true
        let mv = transformPoint (this.Parent:?>LWContainer).Transform.V2W (PointF(single e.X,single e.Y))
        dragPoint <- transformPoint this.Transform.V2W mv
        this.Parent.Invalidate()

and EsButton() =
  inherit LWRectangle()

  let mutable text = ""

  member this.Text
    with get() = text
    and set(v) = text <- v

  override this.OnPaint e =
    base.OnPaint e 
    let parent = this.Parent
    let g = e.Graphics
    let ssz = g.MeasureString(text, parent.Font)
    let p = this.Position
    let sz = this.Size
    let sx, sy = p.X + (sz.Width - ssz.Width) / 2.f, p.Y + (sz.Height - ssz.Height) / 2.f
    g.DrawString(text, parent.Font, Brushes.Black, PointF(sx, sy))

and LetterSquare() as this = 
    inherit SelRectangle()
    let mutable text = ""
        
    let mutable font: Font = FontList.First


    member this.Text
        with get() = text
        and set(v) = text <- v

    member this.Font
        with get() = font
        and set(v) = font <- v

    override this.OnPaint e =
        let g = e.Graphics
        let textColor = match this.Selected with true -> Brushes.Red | false -> Brushes.Black
        base.OnPaint e
        let ssz = g.MeasureString(text, font)
        let p = this.Position
        let sz = this.Size
        let sx, sy = p.X + (sz.Width - ssz.Width) / 2.f, p.Y + (sz.Height - ssz.Height) / 2.f 
        g.DrawString(text, font, textColor, PointF(sx, sy))   

and LetterSquareAnimate() =
    inherit LetterSquare()

    let mutable velocity = PointF(single (System.Random().NextDouble() * 5.), single (System.Random().NextDouble() * 5.))

    member this.Velocity
        with get(): PointF = velocity
        and set(v) = velocity <- v

and CreateButton() =
    inherit EsButton()
    override this.OnMouseDown e =
        base.OnMouseDown e

        let letterSquare = LetterSquareAnimate(
            Parent=this.Parent,
            Position=PointF(35.f , 35.f),
            Size=SizeF(30.f, 30.f),
            Text=(this.Parent:?>KeyCont).Letter,
            DrawColor = Pens.Transparent,
            CoordinateType = World)

        letterSquare.Transform.V2W <- (this.Parent :?> LWContainer).Transform.W2V.Clone()
        letterSquare.Transform.W2V <- (this.Parent :?> LWContainer).Transform.V2W.Clone()


        (this.Parent :?> LWContainer).LWControls.Add(letterSquare)
        this.Parent.Invalidate()
    
    override this.OnPaint e =
        this.Text <- (this.Parent:?>KeyCont).Letter
        base.OnPaint e

type ZoomPlus() =
    inherit EsButton(Text="+")
    override this.OnMouseDown e =
        base.OnMouseDown e
        let parent = this.Parent:?>KeyCont
        match parent.SelectedControl with
            | Some(v) ->
                let centerX = v.Position.X + v.Size.Width / 2.f
                let centerY = v.Position.Y + v.Size.Height / 2.f
                v.Transform.Translate(centerX, centerY)
                v.Transform.Scale(1.1f, 1.1f)
                v.Transform.Translate(-centerX, -centerY)
            | None ->
                let centerX = single this.Parent.Size.Width / 2.f
                let centerY = single this.Parent.Size.Height / 2.f
                let newCenter = transformPoint (this.Parent:?>LWContainer).Transform.V2W (PointF(centerX, centerY))
                (this.Parent:?>LWContainer).Transform.Translate(newCenter.X, newCenter.Y)
                (this.Parent:?>LWContainer).Transform.Scale(1.1f, 1.1f)
                (this.Parent:?>LWContainer).Transform.Translate(-newCenter.X, -newCenter.Y)
        this.Parent.Invalidate()
type ZoomMin() =
    inherit EsButton(Text="-")
    override this.OnMouseDown e =
        base.OnMouseDown e
        let parent = this.Parent:?>KeyCont
        match parent.SelectedControl with
            | Some(v) ->
                let centerX = v.Position.X + v.Size.Width / 2.f
                let centerY = v.Position.Y + v.Size.Height / 2.f
                v.Transform.Translate(centerX, centerY)
                v.Transform.Scale(1.f/1.1f, 1.f/1.1f)
                v.Transform.Translate(-centerX, -centerY)
            | None -> 
                let centerX = single this.Parent.Size.Width / 2.f
                let centerY = single this.Parent.Size.Height / 2.f
                let newCenter = transformPoint (this.Parent:?>LWContainer).Transform.V2W (PointF(centerX, centerY))
                (this.Parent:?>LWContainer).Transform.Translate(newCenter.X, newCenter.Y)
                (this.Parent:?>LWContainer).Transform.Scale(1.f/1.1f, 1.f/1.1f)
                (this.Parent:?>LWContainer).Transform.Translate(-newCenter.X, -newCenter.Y)
        this.Parent.Invalidate()
type MoveUp() =
    inherit EsButton(Text="^")
    override this.OnMouseDown e =
        base.OnMouseDown e
        let parent = this.Parent:?>KeyCont
        match parent.SelectedControl with
            | Some(v) ->
                let l = v.Position
                let vl = transformPoint v.Transform.W2V l
                let nvl = PointF(vl.X, vl.Y - 10.f)
                let wl = transformPoint v.Transform.V2W vl
                let nwl = transformPoint v.Transform.V2W nvl
                let cwl = transformPoint (this.Parent:?>LWContainer).Transform.V2W wl
                let ncwl = transformPoint (this.Parent:?>LWContainer).Transform.V2W nwl
                v.Transform.Translate(ncwl.X - cwl.X, ncwl.Y - cwl.Y)
                
            | None ->
                let parent = (this.Parent:?>LWContainer)
                let l = PointF(0.f, 0.f)
                let vl = transformPoint parent.Transform.W2V l
                let nvl = PointF(vl.X, vl.Y + 10.f)
                let wl = transformPoint parent.Transform.V2W vl
                let nwl = transformPoint parent.Transform.V2W nvl
                parent.Transform.Translate(nwl.X - wl.X, nwl.Y - wl.Y)
        this.Parent.Invalidate()
type MoveDown() =
    inherit EsButton(Text="v")
    override this.OnMouseDown e =
        base.OnMouseDown e
        let parent = this.Parent:?>KeyCont
        match parent.SelectedControl with
            | Some(v) ->
                let l = v.Position
                let vl = transformPoint v.Transform.W2V l
                let nvl = PointF(vl.X, vl.Y + 10.f)
                let wl = transformPoint v.Transform.V2W vl
                let nwl = transformPoint v.Transform.V2W nvl
                let cwl = transformPoint (this.Parent:?>LWContainer).Transform.V2W wl
                let ncwl = transformPoint (this.Parent:?>LWContainer).Transform.V2W nwl
                v.Transform.Translate(ncwl.X - cwl.X, ncwl.Y - cwl.Y)
                
            | None ->
                let parent = (this.Parent:?>LWContainer)
                let l = PointF(0.f, 0.f)
                let vl = transformPoint parent.Transform.W2V l
                let nvl = PointF(vl.X, vl.Y - 10.f)
                let wl = transformPoint parent.Transform.V2W vl
                let nwl = transformPoint parent.Transform.V2W nvl
                parent.Transform.Translate(nwl.X - wl.X, nwl.Y - wl.Y)
        this.Parent.Invalidate()
type MoveLeft() =
    inherit EsButton(Text="<")
    override this.OnMouseDown e =
        base.OnMouseDown e
        let parent = this.Parent:?>KeyCont
        match parent.SelectedControl with
            | Some(v) ->
                let l = v.Position
                let vl = transformPoint v.Transform.W2V l
                let nvl = PointF(vl.X - 10.f, vl.Y)
                let wl = transformPoint v.Transform.V2W vl
                let nwl = transformPoint v.Transform.V2W nvl
                let cwl = transformPoint (this.Parent:?>LWContainer).Transform.V2W wl
                let ncwl = transformPoint (this.Parent:?>LWContainer).Transform.V2W nwl
                v.Transform.Translate(ncwl.X - cwl.X, ncwl.Y - cwl.Y)
                
            | None ->
                let parent = (this.Parent:?>LWContainer)
                let l = PointF(0.f, 0.f)
                let vl = transformPoint parent.Transform.W2V l
                let nvl = PointF(vl.X + 10.f, vl.Y)
                let wl = transformPoint parent.Transform.V2W vl
                let nwl = transformPoint parent.Transform.V2W nvl
                parent.Transform.Translate(nwl.X - wl.X, nwl.Y - wl.Y)
        this.Parent.Invalidate()
type MoveRight() =
    inherit EsButton(Text=">")
    override this.OnMouseDown e =
        base.OnMouseDown e
        let parent = this.Parent:?>KeyCont
        match parent.SelectedControl with
            | Some(v) ->
                let l = v.Position
                let vl = transformPoint v.Transform.W2V l
                let nvl = PointF(vl.X + 10.f, vl.Y)
                let wl = transformPoint v.Transform.V2W vl
                let nwl = transformPoint v.Transform.V2W nvl
                let cwl = transformPoint (this.Parent:?>LWContainer).Transform.V2W wl
                let ncwl = transformPoint (this.Parent:?>LWContainer).Transform.V2W nwl
                v.Transform.Translate(ncwl.X - cwl.X, ncwl.Y - cwl.Y)
                
            | None ->
                let parent = (this.Parent:?>LWContainer)
                let l = PointF(0.f, 0.f)
                let vl = transformPoint parent.Transform.W2V l
                let nvl = PointF(vl.X - 10.f, vl.Y)
                let wl = transformPoint parent.Transform.V2W vl
                let nwl = transformPoint parent.Transform.V2W nvl
                parent.Transform.Translate(nwl.X - wl.X, nwl.Y - wl.Y)
        this.Parent.Invalidate()
type RotateLeft() =
    inherit EsButton(Text="RL")
    override this.OnMouseDown e =
        base.OnMouseDown e
        let parent = this.Parent:?>KeyCont
        match parent.SelectedControl with
            | Some(v) -> 
                let centerX = v.Position.X + v.Size.Width / 2.f
                let centerY = v.Position.Y + v.Size.Height / 2.f
                v.Transform.Translate(centerX, centerY)
                v.Transform.Rotate(-10.f)
                v.Transform.Translate(-centerX, -centerY)
            | None ->
                let centerX = single this.Parent.Size.Width / 2.f
                let centerY = single this.Parent.Size.Height / 2.f
                let newCenter = transformPoint (this.Parent:?>LWContainer).Transform.V2W (PointF(centerX, centerY))
                (this.Parent:?>LWContainer).Transform.Translate(newCenter.X, newCenter.Y)
                (this.Parent:?>LWContainer).Transform.Rotate(10.f)
                (this.Parent:?>LWContainer).Transform.Translate(-newCenter.X, -newCenter.Y)

        this.Parent.Invalidate()
type RotateRight() =
    inherit EsButton(Text="RR")
    override this.OnMouseDown e =
        base.OnMouseDown e
        let parent = this.Parent:?>KeyCont
        match parent.SelectedControl with
            | Some(v) -> 
                let centerX = v.Position.X + v.Size.Width / 2.f
                let centerY = v.Position.Y + v.Size.Height / 2.f
                v.Transform.Translate(centerX, centerY)
                v.Transform.Rotate(10.f)
                v.Transform.Translate(-centerX, -centerY)
            | None ->
                let centerX = single this.Parent.Size.Width / 2.f
                let centerY = single this.Parent.Size.Height / 2.f
                let newCenter = transformPoint (this.Parent:?>LWContainer).Transform.V2W (PointF(centerX, centerY))
                (this.Parent:?>LWContainer).Transform.Translate(newCenter.X, newCenter.Y)
                (this.Parent:?>LWContainer).Transform.Rotate(-10.f)
                (this.Parent:?>LWContainer).Transform.Translate(-newCenter.X, -newCenter.Y)
        this.Parent.Invalidate()
type DelLetter() =
    inherit EsButton(Text="Del")
    override this.OnMouseDown e =
        base.OnMouseDown e
        let parent = this.Parent:?>KeyCont
        match parent.SelectedControl with
            | Some(v) -> 
                (this.Parent:?>LWContainer).LWControls.Remove(v) |> ignore
                parent.SelectedControl <- None
                this.Parent.Invalidate()
            | None -> ()
type ChFont() =
    inherit EsButton(Text="Font")
    override this.OnMouseDown e =
        base.OnMouseDown e
        let parent = this.Parent:?>KeyCont
        match parent.SelectedControl with
            | Some(v) -> 
                let letter = v:?>LetterSquare
                letter.Font <- FontList.Next(letter.Font)
                this.Parent.Invalidate()
            | None -> ()

type AnimateButton() =
    inherit EsButton(Text="Animation")
    override this.OnMouseDown e =
        base.OnMouseDown e
        (this.Parent:?>KeyCont).ToggleAnimation

let container = new KeyCont(Dock=DockStyle.Fill)
let form = new Form(Size=Size(500,300))

let sideBut = 30.f
let sideBar = 34.f

let bar = LWRectangle(Parent=container, 
        Position=PointF(0.f, 0.f), 
        Size=SizeF((sideBut+2.f) * 13.f + 0.f, sideBar),
        // Size=SizeF(single form.Size.Width, sideBar)
        FillColor=Brushes.White,
        DrawColor=Pens.Gray)
let creat = CreateButton(Parent=container, 
        Position=PointF(2.f, 2.f),
        Size=SizeF(sideBut, sideBut),
        FillColor=Brushes.Gray)
let zoomUp = ZoomPlus(Parent=container, 
        Position=PointF(sideBut+4.f, 2.f),
        Size=SizeF(sideBut, sideBut),
        FillColor=Brushes.Gray)
let zoomDown = ZoomMin(Parent=container, 
        Position=PointF((sideBut+2.f) * 2.f + 2.f , 2.f),
        Size=SizeF(sideBut, sideBut),
        FillColor=Brushes.Gray)
let moveUp = MoveUp(Parent=container, 
        Position=PointF((sideBut+2.f) * 3.f + 2.f, 2.f),
        Size=SizeF(sideBut, sideBut),
        FillColor=Brushes.Gray)
let moveDown = MoveDown(Parent=container, 
        Position=PointF((sideBut+2.f) * 4.f + 2.f, 2.f),
        Size=SizeF(sideBut, sideBut),
        FillColor=Brushes.Gray)
let moveLeft = MoveLeft(Parent=container, 
        Position=PointF((sideBut+2.f) * 5.f + 2.f, 2.f),
        Size=SizeF(sideBut, sideBut),
        FillColor=Brushes.Gray)
let moveRight = MoveRight(Parent=container, 
        Position=PointF((sideBut+2.f) * 6.f + 2.f, 2.f),
        Size=SizeF(sideBut, sideBut),
        FillColor=Brushes.Gray)
let rotateLeft = RotateLeft(Parent=container, 
        Position=PointF((sideBut+2.f) * 7.f + 2.f, 2.f),
        Size=SizeF(sideBut, sideBut),
        FillColor=Brushes.Gray)
let rotateRight = RotateRight(Parent=container, 
        Position=PointF((sideBut+2.f) * 8.f + 2.f, 2.f),
        Size=SizeF(sideBut, sideBut),
        FillColor=Brushes.Gray)
let delLetter = DelLetter(Parent=container, 
        Position=PointF((sideBut+2.f) * 9.f + 2.f, 2.f),
        Size=SizeF(sideBut, sideBut),
        FillColor=Brushes.Gray)
let chFont = ChFont(Parent=container, 
        Position=PointF((sideBut+2.f) * 10.f + 2.f, 2.f),
        Size=SizeF(sideBut, sideBut),
        FillColor=Brushes.Gray)

let square = LWRectangleNoHit(Parent=container, 
        Position=PointF(35.f, 35.f),
        Size=SizeF(30.f, 30.f),
        DrawColor=Pens.Black)

let animate = AnimateButton(Parent=container, 
        Position=PointF((sideBut+2.f) * 11.f + 2.f, 2.f),
        Size=SizeF(sideBut * 2.f, sideBut),
        FillColor=Brushes.Gray)



container.LWControls.Add(creat)
container.LWControls.Add(zoomUp)
container.LWControls.Add(zoomDown)
container.LWControls.Add(moveUp)
container.LWControls.Add(moveDown)
container.LWControls.Add(moveLeft)
container.LWControls.Add(moveRight)
container.LWControls.Add(rotateLeft)
container.LWControls.Add(rotateRight)
container.LWControls.Add(delLetter)
container.LWControls.Add(chFont)
container.LWControls.Add(square)
container.LWControls.Add(animate)

container.LWControls.Add(bar)


form.Controls.Add(container)

form.Show()