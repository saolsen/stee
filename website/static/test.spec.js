// Tests for stee
function eval_main_program(name, src, result) {
  console.log(src);
  it(name, function(done) {
    try {
      compile(src, {}).then(r => {
        expect(r.instance.exports.main()).toBe(result);
        done();
      });
    } catch(e) {
      expect(e).toBe(null);
      done();
    }
  });
}

describe("Stee", function() {
  jasmine.DEFAULT_TIMEOUT_INTERVAL = 11000;

  var compile;

  beforeAll(function(done){
    setTimeout(function() {   
        compile = window.compile;
        done();
    }, 100);
  });

  it("load compiler", function() {
       expect(compile).toBeDefined();
  });

  eval_main_program("simple add", `
    func add(i: i32, j: i32): i32 {
        var foo: i32;
        var bar: i32;
        foo = i + j;
        bar = i - j;
        return foo;
    }
    export func main() : i32 {
        return add(1,2) + add(3,4);
    }`, 10);

  

  it("compile error", function(done) {
    let src = `
    fu
    `;

    try {
      compile(src, {}).then(result => {
        expect(1).toBe(2); // Error, shouldn't have been compiled.
        done();
      });
    } catch(e) {
      expect(true).toBe(true);
      done();
    }
  });

  it("call import", function(done) {
    let src = `
    import func foo(x: i32) : i32;
    export func main() : i32 {
      return foo(5);
    }
    `;

    let importObject = {
      env: {
        foo: function(i) { return i + 1; }
      }
    };

    try {
      console.log(importObject);
      compile(src, importObject).then(r => {
        window.r = r;
        expect(r.instance.exports.main()).toBe(6);
        done();
      });
    } catch(e) {
      expect(e).toBe(null);
      done();
    }
  });

});