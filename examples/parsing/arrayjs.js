/*!
 * arrayJS JavaScript Library v0.2.1
 * 
 * http://b-studios.de
 *
 * Copyright 2010, Jonathan Brachthäuser
 * Dual licensed under the MIT or GPL Version 2 licenses.
 * http://b-studios.de/licence
 *
 * Date: Sat Nov 27 18:03:45 2010 +0100
 *
 * Changes from v0.2
 * - Fixed filter method. filter$ did not work properly because of splicing
 * - added copy()
 * - fixed uniq(), ie had problems because of indexOf
 * - optimized uniq$, got chrome from 1.5s down to 11ms and IE from 15s to 362ms 
 * - fixed some IE-Issues with fastUniq
 */ 
/**
 * @function arrayjs
 *
 * Factorymethod for collections.git 
 * 
 * @overload 
 *   @param [Array] elements The elements which are used, to form the collection
 *   @return [arrayjs.Collection]
 *
 * @overload
 *   Sometimes it is easier to separate items of an array with whitespaces in a 
 *   string, so this is also possible: `_("foo bar yeah this works!");`
 *      
 *   @param [String] elements Each whitespace seperated section of the string will
 *     be interpreted as an element of an array.
 *   @return [arrayjs.Collection]
 
 * @overload
 *   Variadic function with multiple arguments. Every argument is	considered to 
 *   be an element of the array.
 *   @param [Object] el1
 *   @param [Object] el2
 *   @param [...] ...
 *   @return [arrayjs.Collection]
 *
 */
var arrayjs = function(elements) {

	var applyToCopy = function(old, method) {	
		return function() {
			var copy = new Collection(old.toArray());
			method.apply(copy, arguments);
			return copy;
		}			
	}
  
  /**
   * @object arrayjs.Collection
   *
   * For information on how to retreive a Collection-Object from your list of
   * elements see {arrayjs the factory method}.
   *
   * Array.prototype inheritance
   * ---------------------------
   * All default Array-Operations can be used also on a arrayJS-Collection.
   * Implemented in Array by default:		 
	 * 
	 * - .push
	 * - .pop
	 * - .shift
	 * - .unshift		 
	 * - {.reverse}
	 * - .slice
	 * - .splice
	 * - .sort
	 * - {.filter}
   * 
   * Iterating
   * ---------
   * - {.each}
   * - {.collect .collect/.map}
   *
   * Filtering
   * ---------
   * - {.compact}
   * - {.filter .filter/.select}
   * - {.reject}
   * - {.uniq} / {.fastUniq}
   * 
   * Working with other Collections
   * ------------------------------
   * - {.concat .concat/.cat}
   * - {.cut}
   * - {.dif} 
   *
   * Single Element Access & Manipulation
   * ------------------------------------
   * - {.deleteAt .deleteAt/.removeAt}
   * - {.first}
   * - {.index}
   * - {.insert}
   * - {.item}
   * - {.last}
   * - {.remove}
   * - {.replace}
   * - {.valuesAt}
   * 
   *
   * Retreiving Information
   * ----------------------
   * - {.contains}
   * - {.count}
   * - {.isEmpty}
   * - {.size}
   *
   * Global Manipulation
   * -------------------
   * - {.clear}
   * - {.copy}
   * - {.reverse}
   * - {.toArray}
   * - {.toString}
   *
   */
	var Collection = function(elements) {				
		
		this.isCollection = true;	
			
		/** 
		 * provide indexOf Method if it does not exist. (Internet Explorer)
		 */
		if( !Array.prototype.indexOf ) {			
			this.indexOf = function(element) {			
				for(var i = 0; i < this.length; i++) {				
					if(this[i] == element) {
						return i;
					}				
				}	
				return -1;				
			}
		}
	
		/**
		 * @method .each
		 *
		 * Iterates over all elements of this collection. Block gets element as 
		 * `this`-context. Using the second argument `el` is recommended, due to
		 * performance issues in IE.
		 *
		 * @param callback 
		 *   [Number] i The index of the current iteration
		 *   [Object] el The current element 
		 */
		this.each = function(block) {
			for(var i = 0; i < this.length; i++) {					
				block.call(this[i], i, this[i]);
			}			
			return this;			
		};
		
	  /**
	   * @method .collect
		 *
	 	 * Modifies {.each each} element in existing array using the specified block.
		 * 
		 * @example Usage
		 *   _([1,5,6,7]).collect(function(i,el) {
		 *     return this*2;
		 *   });
		 *     
		 *   -> _(2,10,12,14)
		 *
		 * @param callback 
		 *   [Number] i The index of the current iteration
		 *   [Object] el The current element 
		 * @return [.Collection] a new collection-object containing all returned
		 *   values
		 */
		this.collect$ = function(block) {
			
			var that = this;
			return this.each(function(i) {
				that[i] = block.call(that[i], i, that[i]);
			});
		}
		this.collect = applyToCopy(this, this.collect$);
		

	  /** 
	   * @method .map
		 * Facade for {.collect}
		 */
		this.map$ = this.collect$;
		this.map = this.collect;		
		
	  /**
	   * @method .filter
	   *
	 	 * Own implementation of [].filter(), because of consistency in use.
	 	 *
	 	 * `block` has to return true if `element` should be kept in the Collection.
	 	 * 
	 	 * @example Usage
	 	 *   $([1,5,6,7]).filter(function(i,el) {
	 	 *     return this > 5;
	 	 *   });
	 	 *   
	 	 *   > [6, 7]
	 	 * 
	 	 * @param block
	 	 *   [Number] i current index
	 	 *   [Object] el current element
	 	 *
	 	 * @note FIXED: splice reduced the length, but each won't take notice of it.
	 	 */
		this.filter$ = function(block) {
			for(var i = 0; i < this.length; i++) {			
				if( !block.call(this[i],i,this[i]) )
					this.splice(i--,1);
			};
			return this
		}
		this.filter = function(block) {		
			var elements = [];			
			
			this.each( function(i, el) {					
				if( block.call(el, i, el) ) 
					elements.push(el);							
			});					
			
			return new Collection(elements);				
		}
		
		/** 
		 * @method .select
		 * Same as {.filter}
		 */
		this.select$ = this.filter$;
		this.select = this.select;

		/**
		 * @method .reject
		 *
		 * Inverted-{.filter filter}
		 */
		this.reject$ = function(block) {
			return this.filter$(function(i, el) {
				return !block.call(this, i, el);
			});
		}
		this.reject = function(block) {
			return this.filter(function(i, el) {
				return !block.call(this, i, el);
			});
		}		
		
		/**
		 * @method .replace
		 *
		 * Replaces all occurances of toReplace with otherItem
		 *
		 * @param [Object] toReplace
		 * @param [Object] otherItem
		 */
		this.replace$ = function(toReplace, otherItem) {
			var that = this;
			return this.each(function(i,el) {
				if(that[i] == toReplace)
					that[i] = otherItem;			
			});
		}
		this.replace = applyToCopy(this, this.replace$);
		
	  /**
	   * @method .compact
	   *
	 	 * Removes all empty nodes (null or undefined) from Array
		 */
		this.compact$ = function() {
			return this.filter$(function(i, el) {
				return !(el == undefined || el == null);
			});
		}
		this.compact = function() {
			return this.filter(function(i, el) {
				return !(el == undefined || el == null);
			});
		}				

	  /**
	   * @method .remove 
	   * 
		 * Removes matching elements from Array
		 *
		 * @param [Object] element element to remove
		 * @note Idea: use multiple arguments to specify, which items to be removed
		 *   or: use second argument for comparison function
		 */
		this.remove$ = function(element) {
			return this.filter$(function(i,el) {
				return el != element;
			});
		}
		this.remove = function(element) {		
			return this.filter(function(i,el) {
				return el != element;
			});
		}
		
		/**
		 * @method .deleteAt 
		 *
		 * Deletes the element at the specified index
		 * @param [Number] index
		 */
		this.deleteAt$ = function(index) {
			this.splice(index, 1);
			return this;
		}
		this.deleteAt = applyToCopy(this, this.deleteAt$);		
		
		/**
		 * @method .removeAt
		 * Same as {.deleteAt}
		 */
		this.removeAt$ = this.deleteAt$;
		this.removeAt = this.deleteAt;
		
		/**
		 * @method .uniq
		 *
		 * Removes all duplicate entries from array. Currently not very performant. 
		 * Use {.fastUniq} instead, if you are experience problems.
		 * This algorithm has `O(n²)`
		 */
		this.uniq$ = function() {
			var elements = arrayjs();
			
			for(var i = 0; i < this.length; i++) {				
				if(elements.indexOf(this[i]) == -1)
					elements.push(this[i]);
			}
			
			this.clear();
			Array.prototype.push.apply(this, elements.toArray());
			return this;
		}
		this.uniq = function() {
			var elements = arrayjs();
			
			for(var i = 0; i < this.length; i++) {				
				if(elements.indexOf(this[i]) == -1)
					elements.push(this[i]);
			}
			
			return elements;
		}
		
		/**
		 * @method .fastUniq
		 *
		 * Faster version of {.uniq} with complexity `O(n*log(n) + n)`, but also
		 * sorts the collection.
		 *
		 * The comparison-block is optional, but maybe needed for complex objects or 
		 * arrays to work
		 *
		 * @param comparison?
		 *   [Object] element1 
		 *   [Object] element2
		 * @return [arrayjs.Collection] sorted and unique array
		 */
		this.fastUniq$ = function(comparison) {
			var that = this;
			// IE won't work with undefined comparison as argument, so we need
			// this if-clause. Further it does not work with this.sort - so we
			// use Array.prototype.sort
			if(comparison)
				Array.prototype.sort.call(this, comparison);
			else 
				Array.prototype.sort.call(this);
			
			for(var i = 0; i < this.length; i++) {
				if(i > 0 && that[i-1] == that[i])
					that.splice(i--,1);			
			}
			return this;				
		}
		this.fastUniq = applyToCopy(this, this.fastUniq$);
		
		/**
		 * @method .insert
		 *
		 * Accepts variable argumentlength.
		 *
		 * @param [Number] index index to insert at. All following
		 *   elements are inserted at this place.
		 */		
		this.insert$ = function(index) {
		
			if(arguments.length < 2) 
				throw "Please specify index and element(s) to insert."
				
			Array.prototype.splice.call(arguments, 1,0,0);
			Array.prototype.splice.apply(this, arguments);
			return this;
		}
		this.insert = applyToCopy(this, this.insert$);		
		
	  /** 
	   * @method .reverse
	   *
		 * Reverse could be inherited from Array.prototype, but then the reversal would be saved
		 * to original Collection and not a new one would be returned
		 */
		this.reverse$ = function() { 
			Array.prototype.reverse.apply(this);
			return this;
		}
		this.reverse = applyToCopy(this, this.reverse$);
			
		/**
		 * @method .dif
		 *
		 * Removes all items from this collection, which are contained in otherArray and therefore 
		 * creating the difference
		 * 
		 * @param [arrayjs.Collection] other
		 */
		this.dif$ = function(otherArray) {
			var other = arrayjs(otherArray);
			for(var i = 0; i < this.length; i++) {
				if(other.contains(this[i]))
					this.splice(i--,1);
			}
			return this;
		}
		this.dif = applyToCopy(this, this.dif$);
		
		/**
		 * @method .cut
		 *
		 * Returns the cut (intersection) of this array with `other`. 
		 * To be a little more efficient it does NOT prevent duplicates. This results 
		 * in O(n²) instead of O(n³)
		 *
		 * @param [arrayjs.Collection] other
		 */		 
		this.cut$ = function(otherArray) {			
			var other = arrayjs(otherArray);
			for(var i = 0; i < this.length; i++) {
				if(!other.contains(this[i]))
					this.splice(i--,1);
			}
			return this;
		}
		this.cut = applyToCopy(this, this.cut$);	
	
	  /**
		 * @method .concat 
		 *
		 * Attaches the elements of `other` to the end of this collection
		 *
		 * @example 
		 *   var foo = _(5, 9, 4, 2);
		 *   var bar = _(8, 7, 1);
		 *   foo.concat(bar); //-> [5, 9, 4, 2, 8, 7, 1]
		 *
		 * @param [arrayjs.Collection] other
		 * @return [arrayjs.Collection] new Collection containing both `this` and `other`
		 */
		this.concat$ = function(other) { 
			Array.prototype.push.apply(this, this.toArray.call(other));
			return this;
		}
		this.concat = function(other) {
			// the second argument of concat intentionally uses this.toArray, because it could be
			// a real array and therefore doesn't have the method 'toArray'
			return new Collection(Array.prototype.concat.call(this.toArray(), this.toArray.apply(other)));
		}
		this.cat$ = this.concat$;
		this.cat = this.concat;
		
	
		/**
		 * @method .clear
		 *
		 * @warn This method really empties THIS-collection.
		 * @note Because of the side-effects, it should be named clear$, but that 
		 *   maybe a little confusing, because there is not clear without side-effects.
		 *
		 * @return [arrayjs.Collection] empty collection
		 */
		this.clear = function() {
		// have to check this in IE!!!
		Array.prototype.splice.call(this, 0, this.length);
		return this;
		}
		this.empty = this.clear;
	
	
   //------------------------------- NOT CHAINING METHODS -----------------------------------------	
	
	 /**
	  * @method .contains
	  *
		*	Finds first occurance of element and returns true if found and false if not.
		*
		* @param [Object] element
		* @return [Boolean]
		*/
		this.contains = function(element) {			
			return this.indexOf(element) == -1? false: true;	
		}	
		
		/**
		 * @method .index
		 *
		 * Get's the index of the searched element. Facade for indexOf, but returns null, if the
		 * element is not found.
		 *
		 * @param [Object] element
		 * @return [Number] the found index or -1
		 */
		this.index = function(element) {
			var index = this.indexOf(element);
			return index == -1? null :index;			
		}
		
	  /**
	   * @method .count
	   *
		 * Counts the occurances of `element` in this collection
		 *
		 * @param [Object] element
		 * @return [Number] Number of found `element`s
		 */
		this.count = function(element) {
			return this.filter(function(i,el) {
				return element == el;
			}).length;		
		}
		
		/**
		 * @method .toArray
		 *
		 * Probably fastest way to convert to real Array.
		 *
		 * @note Should multidimentsional arrays be converted to multidimensional
		 *   collections in the first place and then reconverted to multidimensional
		 *   native arrays with this methods???
		 * 
		 * @return [Array] the equivalent 'real' Array
		 */
		this.toArray = function() {					
			return Array.prototype.slice.call(this);
		}	 
		
		/**
		 * @method .item
		 *
		 * Facade for Array-like-access:
		 *
		 *    collection.item(3) === collection[3] //-> true
		 *
		 * @param [Number] index
		 * @return [Object] the item at position `index`
		 */
		this.item = function(index) {
			return this[index];
		}
		
		/**
		 * @method .first
		 *
		 * @return [Object] first element of this collection
		 */
		this.first = function() {
			return this[0];
		}
		
		/**
		 * @method .last
		 *
		 * @return [Object] last element of this collection
		 */
		this.last = function() {
			return this[this.length-1];
		}
		
		/**
		 * @method .valuesAt
		 *
		 * Returns values, which are specified at indices. Mutliple arguments are 
		 * allowed. The values are returned in a new collection.
		 *
		 * @example 
		 *   _(1,5,7,8,9).valuesAt(0,2,3);
		 *   > [1,7,8]
		 *
		 * @param [Number] index1
		 * @param [Number] index2
		 * @param [...] ...
		 * 
		 * @return [arrayjs.Collection] A new collection, containing the selected
		 *   values.
		 */
		this.valuesAt = function() {		
			var elements = new Collection();						
			for(var i = 0; i < arguments.length; i++) {
				elements.push(this[arguments[i]]);
			}
			return elements;
		}	
		
		/**
		 * @method .isEmpty
		 *
		 * @return [Boolean] true, if this collection is empty
		 */
		this.isEmpty = function() { return this.length == 0; }
		
		/**
		 * @method .size
		 * 
		 * Same as this.length
		 *
		 * @return [Number] the size of this collection.
		 */
		this.size = function() { return this.length; }		
		
		/**
		 * @method .copy
		 *
		 * Can be used to get copy of this collection and work modifying on that,
		 * whithout influencing original collection.
		 *
		 * @return [arrayjs.Collection] Shallow Copy of the underlying array
		 */
		this.copy = function() {
		  return arrayjs(this);
		}
		
		/**
		 * @method .toString
		 * Generates a String-representation of this Collection
		 * 
		 * @return [String] "_[element0, element1, element 2 etc.]"
		 */
		this.toString = function() {
			return '_['+ this.join(', ') +']';
		}		
		
	 /**
		* Thanks to John Resig (jQuery):
		*
		*	 "Resetting the length to 0, then using the native Array push
		*	  is a super-fast way to populate an object with array-like 
		*	  properties."
		*/
		this.length = 0;
		Array.prototype.push.apply(this, elements);
		
		return this;
	}
	// Now Collection looks like a real Array!
	Collection.prototype = Array.prototype;	
	
	// no arguments given
	if(elements == undefined)
		elements = [];		
		
	// It's a string
	if(elements.charAt != undefined)
	  elements = elements.split(' ');
	
	// it's already a Collection-Object
	if(elements && elements.isCollection)
		return elements;
	
	// There are multiple arguments, so tread them as array
	// fix: the first argument was an array itself...
	if(arguments.length > 1) {	
		//arguments[0] = arguments[0][0];	
		return new Collection(Array.prototype.slice.call(arguments));
	} else {
		return new Collection(elements);	
	}	
}
var _ = _ || arrayjs;
