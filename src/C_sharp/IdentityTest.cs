using System;
using System.Diagnostics;
namespace Task35
{
	public class IdentityTest
	{

		public object thing1;

		public object thing2;

		public int trials;

		public IdentityTest (object _thing1, object _thing2, int _trials)
		{
			thing1 = _thing1;
			thing2 = _thing2;
			trials = _trials;
		}

		public long[] MilliTest ()
		{
			var sw = new Stopwatch();
			sw.Start();
			for (var i = 0; i < trials; i++) {
				Object.ReferenceEquals(thing1, thing2);
			}
			sw.Stop();
			return new long[] { sw.ElapsedMilliseconds, trials };
		}

		public long[] NothingTest ()
		{
			var sw = new Stopwatch();
			sw.Start();
			for (var i = 0; i < trials; i++) {
				// nothin!
			}
			sw.Stop();
			return new long[] { sw.ElapsedMilliseconds, trials };
		}

		public long GiveALong ()
		{
			// might get inlined...
			long theLong = 0L;
			theLong = theLong + 0L;
			return theLong;
		}

		public long[] MethodTest ()
		{
			var sw = new Stopwatch();
			sw.Start();
			for (var i = 0; i < trials; i++) {
				GiveALong();
			}
			sw.Stop();
			return new long[] { sw.ElapsedMilliseconds, trials };
		}

		public class Vlumper
		{
			public Vlumper ()
			{
			}

			public string Vlump ()
			{
				return "bevlumped!";
			}
		}

		public class OtherVlumper : Vlumper
		{
			public OtherVlumper ()
			{
			}

			public string Vlump ()
			{
				return "other vlumped";
			}
		}

		public static string VlumpTestInner (Vlumper v1)
		{
			return v1.Vlump();
		}

		public static string VlumpTest ()
		{
			var v = new OtherVlumper();
			return VlumpTestInner(v);
		}

		public long[] MapLookupTest1  (clojure.lang.IPersistentMap map, object key)
		{
			var sw = new Stopwatch();
			sw.Start();
			for (int i = 0; i < trials; i++) {
				map.valAt(key);
			}
			sw.Stop();
			return new long[] { sw.ElapsedMilliseconds, trials };
		}

		public long[] MapLookupTest2 (clojure.lang.IPersistentMap map, object key, clojure.lang.Var GetVar)
		{
			var sw = new Stopwatch();
			sw.Start();
			for (int i = 0; i < trials; i++) {
				((clojure.lang.IFn)GetVar.getRawRoot()).invoke(map, key);
			}
			sw.Stop();
			return new long[] { sw.ElapsedMilliseconds, trials };
		}

		public long[] JumpMapLookupTest1 (Arcadia.JumpMap jm, object key)
		{
			Arcadia.JumpMap.KeyVal kv = jm.Subscribe(key);
			var sw = new Stopwatch();
			sw.Start();
			for (int i = 0; i < trials; i++) {
				kv.GetVal();
			}
			sw.Stop();
			return new long[] { sw.ElapsedMilliseconds, trials };
		}
				
	}
}
