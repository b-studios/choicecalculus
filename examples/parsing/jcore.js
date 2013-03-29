/**
 * I gained much inspiration from Ext 4.0 and YUI. Also much Kudos to Nicolas
 * Zakas and Douglas Crockford for their amazing Books about the real JavaScript!
 *
 * If you like this, maybe you will like joose as well
 * @todo compare with docjs-jcore
 * this version has a fixed prototypal system for inheritence
 */
var global = this;

var jCore = {
  version: 1.0,
  global: global
};

/**
 * @object J
 */
var J = J || jCore;

(function(J) {
  
  // Adding reflection to Objects
  var test_for = function(args) {
    return function(test) {
      for(var i=0, len=args.length; i<len; i++) {
        if(args[i] == test)
          return true;        
      }
      return false;
    };  
  };
    
  if(Object.prototype.is_a === undefined)
    Object.prototype.is_a = function(test) { 
          
      switch(test) {
        case 'Function':
          return typeof this === 'function'; 
          
        case 'Object':
          return true;
      }
      return false;
    };
    
  if(!Array.prototype.hasOwnProperty('is_a'))
    Array.prototype.is_a = test_for(["Array", "Object"]);

  if(!String.prototype.hasOwnProperty('is_a'))
    String.prototype.is_a = test_for(["String", "Object"]);
  
  if(!Number.prototype.hasOwnProperty('is_a'))
    Number.prototype.is_a = test_for(["Number", "Object"]);
    
  if(!Boolean.prototype.hasOwnProperty('is_a'))
    Boolean.prototype.is_a = test_for(["Boolean", "Object"]);
  
  if(!RegExp.prototype.hasOwnProperty('is_a'))
    RegExp.prototype.is_a = test_for(["RegExp", "Function", "Object"]);
    
  if(!Date.prototype.hasOwnProperty('is_a'))
    Date.prototype.is_a = test_for(["Date", "Object"]);


  /**
   * @function J.apply
   *
   * Takes two or more arguments and merges all properties to the first one. (The first object will
   * be modified!)
   * Properties of the second argument will override properties of the first one.
   *
   * @example
   *   var fruits = {
   *     banana: 3,
   *     apple: 12   
   *   };
   *   var computers = {
   *     microsoft: 5,
   *     apple: 18
   *   };
   *   
   *   J.apply(fruits, computers);
   *   console.log(fruits); #=> { banana: 3, apple: 18, microsoft: 5 }
   *
   * @example no deep apply
   *   var person = {     
   *     name: "Jonathan",
   *     contact: {
   *       phone: "0123456789"
   *     }
   *   }
   *   
   *   J.apply(person, {
   *     contact: {
   *       url: "http://b-studios.de"
   *     }
   *   });
   *   console.log(person); #=> { name: "Jonathan", contact: { url: "http://b-studios.de" } }
   *
   * @param [Object] first
   * @param [Object] second
   */
  J.apply = function() {
    
      var original = Array.prototype.shift.apply(arguments);
          extensions = arguments;
    
      if(extensions.length == 0)
        return original;
        
      for(var i=0, len=extensions.length; i<len; i++) {
        for(var key in extensions[i]) {
          if(extensions[i].hasOwnProperty(key))
            original[key] = extensions[i][key];
        }  
      }
      return original;
    };
  
  /**
   * @function J.create
   *
   * @example
   *   J.create("Person", {
   *     constructor: function(name) {
   *       this.name = name;
   *     }, 
   *     say_name: function() { return this.name; }
   *   });
   *
   * @example Using revealing module pattern
   *   J.create("Person", {
   *     constructor: function(name) {
   *       return {
   *         say_name: function() { return name; }
   *       };
   *     }
   *   });
   *
   * @example Extending an object
   *   J.create("Student", Person
   *
   * Also mitigates the problem of using 'new' to create new instances. After 
   * creating the class Foo with creat, the following versions of instantiation 
   * are totally equivalent:
   *
   *     Foo("bar");
   *     new Foo("bar");
   *
   * @overload  
   *   Creates a class called `path` and extends it with the given options
   *   
   *   @param [String] path
   *   @param object
   *     [String] child 
   *
   * @overload  
   *   Creates a class `path`, that inherits from `parent` and will be extended with the given
   *   options
   *   
   *   @param [String] path
   *   @param [Object] parent
   *   @param [Hash] options (see other overload)
   */
  J.create = function(path, parent, options) {
    
    // overload J.create(path, options)
    // implicitly inherits from Object.prototype
    if(arguments.length == 2) {
      options = parent;
      parent = Object;
    }
      
    options = options || {};
    
    var parts   = path.split('.'),
        name    = parts.pop(),
        context = global;
        
    for(var i=0, len=parts.length; i<len; i++) {
      var part = parts[i];
      context[part] = context[part] || {};
      context = context[part]; 
    }
    
    // We use a temporary object as prototype
    var F = function(){};
    F.prototype = parent.prototype;
    
    // now we create our subclass and assign the prototype
    var Class = function() {};    
    Class.prototype = new F();
    
    // we introduce a second constructor to prevent the need for "new"
    var constructor = function() {
    
      var instance = new Class();
    
      // return at this place enables revealing module pattern
      if(options.hasOwnProperty('constructor') && typeof options.constructor === 'function')
        return options.constructor.apply(instance, arguments) || instance;
        
      return instance;
    };
    constructor['new'] = constructor;
    constructor.is_a = test_for(["Class", "Function", "Object"]);
    
    // apply all options to prototype
    J.apply(Class.prototype, options, {
      constructor: constructor,
      superclass: parent.prototype,
      is_a: function(test) {
        return test == name || test == path || parent.prototype.is_a(test);
      }
    });    
    constructor.prototype = Class.prototype;

    // save to specified context and return constructor
    context[name] = constructor;
    return constructor;
  };

})(jCore);
