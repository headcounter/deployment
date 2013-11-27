package;

import hase.display.Animation;
import hase.display.Symbol;

class Digit extends hase.display.Sprite
{
    private var digit:Float;
    private var final_digit:Null<Int>;

    public function new(digit:Int = 0)
    {
        super();
        this.digit = 0;
        this.final_digit = 0;

        this.set_ascii([
            mkrow(" ".code, "_".code, " ".code, 0, 9, 0),
            mkrow("|".code, "-".code, "|".code, 1, 8, 1),
            mkrow("|".code, "0".code, "|".code, 1, 8, 1),
            mkrow("|".code, "-".code, "|".code, 1, 8, 1),
            mkrow(" ".code, "~".code, " ".code, 0, 9, 0),
        ]);
    }

    public function switch_to(digit:Int)
    {
        if (digit != this.digit)
            this.final_digit = digit;
    }

    private inline function
        mkrow(s1:Int, s2:Int, s3:Int, c1:Int, c2:Int, c3:Int):Array<Symbol>
        return [new Symbol(s1, c1), new Symbol(s2, c2), new Symbol(s3, c3)];

    private inline function
        roll(s1:Int, s2:Int, s3:Int, c1:Int, c2:Int, c3:Int):Void
    {
        this.ascii.set(1, 1, new Symbol(s1, c1));
        this.ascii.set(1, 2, new Symbol(s2, c2));
        this.ascii.set(1, 3, new Symbol(s3, c3));
    }

    private function transition(from:Int, to:Int, percent:Float):Void
    {
        var fsym:Int = "0".code + from;
        var tsym:Int = "0".code + to;

        if (percent > 0.9)
            this.roll("-".code, tsym, "-".code,  8, 11,  8);
        else if (percent > 0.8)
            this.roll("~".code, tsym, "~".code,  8, 11,  8);
        else if (percent > 0.6)
            this.roll(tsym, "~".code, fsym,      3,  7,  3);
        else if (percent > 0.5)
            this.roll(tsym, "-".code, fsym,      3,  7,  3);
        else if (percent > 0.4)
            this.roll(tsym, "_".code, fsym,      3,  7,  3);
        else if (percent > 0.2)
            this.roll("_".code, fsym, "_".code,  8, 11,  8);
        else
            this.roll("-".code, fsym, "-".code,  8, 11,  8);

        this.set_dirty();
    }

    public override function update(td:Float):Void
    {
        if (this.final_digit != null) {
            var step:Float = td * 0.005;
            var dir:Int = this.final_digit > this.digit ? 1 : -1;

            var old_digit:Int = Std.int(
                dir == 1 ? this.digit : Math.ceil(this.digit)
            );
            var new_digit:Int = old_digit + dir;

            var percent:Float = 1.0;

            if (Math.abs(new_digit - this.digit) <= step) {
                this.digit = new_digit;
                if (new_digit == this.final_digit)
                    this.final_digit = null;
            } else {
                percent = Math.abs(this.digit - old_digit);
                this.digit += step * dir;
            }

            this.transition(old_digit, new_digit, percent);
        }

        super.update(td);
    }
}

class Counter extends hase.display.Sprite
{
    private static var MAX_DIGITS:Int = 5;
    private var number:Float;
    private var digits:Array<Digit>;

    public function new()
    {
        super();
        this.number = 0;

        this.digits = new Array();
        for (i in 0...Counter.MAX_DIGITS) {
            var digit:Digit = new Digit();
            digit.x += (Counter.MAX_DIGITS - 1 - i) * 2;
            this.digits.push(digit);
            this.add_child(digit);
        }
    }

    private function set_number(number:Int):Void
    {
        for (digit in 0...Counter.MAX_DIGITS) {
            var val:Int = Std.int(number / Math.pow(10, digit)) % 10;
            this.digits[digit].switch_to(val);
        }
    }

    public override function update(td:Float):Void
    {
        var step:Float = td * 0.005;
        var newnum:Int = Std.int(this.number + step);
        if (newnum > Std.int(this.number)) {
            this.set_number(newnum);
        }

        this.number += step;
        super.update(td);
    }
}

class Head extends hase.display.Sprite
{
    private var head_normal:Animation;
    private var head_annoyed:Animation;
    private var timer:Null<Float>;

    public function new()
    {
        super();
        this.head_normal = Animation.from_file("gfx/head.cat");
        this.head_annoyed = Animation.from_file("gfx/head_annoyed.cat");
        this.head_annoyed.fps = 5;
        this.head_annoyed.z = -1;
        this.add_child(this.head_normal);
        this.timer = 60;
    }

    public override function update(td:Float):Void
    {
        if (this.timer != null) {
            if (this.timer <= 0) {
                this.remove_child(this.head_normal);
                this.add_child(this.head_annoyed);
                this.timer = null;
            } else {
                this.timer -= td * 0.001;
            }
        }
        super.update(td);
    }
}

class Center extends hase.display.Sprite
{
    private var head:hase.display.Sprite;
    private var counter:Counter;
    private var coming_soon:Animation;

    public function new()
    {
        super();
        this.head = new Head();
        this.counter = new Counter();
        this.counter.x += 7;
        this.counter.y -= 1;
        this.head.add_child(this.counter);

        this.coming_soon = Animation.from_file("gfx/coming_soon.cat");
        this.coming_soon.y += 16;

        this.add_child(head);
        this.add_child(coming_soon);
    }
}

class Headcounter implements hase.Application
{
    public function init():Void
    {
        var center:hase.display.Sprite = new Center();
        center.x = Std.int((this.root.width - center.width) / 2);
        center.y = Std.int((this.root.height - center.height) / 2);
        this.root.add_child(center);

        js.Browser.document.onclick = function(_) {
            js.Browser.document.location.href =
                "https://jabber.headcounter.org/";
        };
    }

    public function update(td:Float):Void {}
}
