

import junit.framework.TestCase;
import com.legstar.test.cixs.binnatsi.*;
import com.legstar.test.coxb.binnatsi.*;

public class POJOClientBinnatsiTest extends TestCase {

	public void testTypesmix() throws BinnatsiFault {
		Binnatsi port = new BinnatsiImpl(); 
		DfhcommareaType dfhcommarea = new DfhcommareaType();
		LsUnsignedNativeType lsUnsignedNative = new LsUnsignedNativeType();
		dfhcommarea.setLsUnsignedNative(lsUnsignedNative);
		
		LsHalfwordsType lsHalfworld = new LsHalfwordsType();
		lsUnsignedNative.setLsHalfwords(lsHalfworld);
		lsHalfworld.setLsPs9X4High(Short.parseShort("1045"));
		lsHalfworld.setLsPs9X4Low(Short.parseShort("-128"));
		lsHalfworld.setLsPs9X4Max(Short.parseShort("32767"));
		lsHalfworld.setLsPs9X4Min(Short.parseShort("-32768"));
		
		LsFullwordsType lsFullworld = new LsFullwordsType();
		lsUnsignedNative.setLsFullwords(lsFullworld);
		lsFullworld.setLsPs9X9High(Integer.parseInt("123456789"));
		lsFullworld.setLsPs9X9Low(Integer.parseInt("-128"));
		lsFullworld.setLsPs9X9Max(Integer.parseInt("2147483647"));
		lsFullworld.setLsPs9X9Min(Integer.parseInt("-2147483648"));
		
		LsDoublewordsType lsDoubleworld = new LsDoublewordsType();
		lsUnsignedNative.setLsDoublewords(lsDoubleworld);
		lsDoubleworld.setLsPs9X18High(Long.parseLong("17179869183"));
		lsDoubleworld.setLsPs9X18Low(Long.parseLong("-4294967294"));
		lsDoubleworld.setLsPs9X18Max(Long.parseLong("9223372036854775807"));
		lsDoubleworld.setLsPs9X18Min(Long.parseLong("-9223372036854775808"));
		
		DfhcommareaType resp = port.binnatsi(dfhcommarea, null);

		assertEquals(1045, resp.getLsUnsignedNative().getLsHalfwords().getLsPs9X4High());
		assertEquals(-128, resp.getLsUnsignedNative().getLsHalfwords().getLsPs9X4Low());
		assertEquals(32767, resp.getLsUnsignedNative().getLsHalfwords().getLsPs9X4Max());
		assertEquals(-32768, resp.getLsUnsignedNative().getLsHalfwords().getLsPs9X4Min());

		assertEquals(123456789, resp.getLsUnsignedNative().getLsFullwords().getLsPs9X9High());
		assertEquals(-128, resp.getLsUnsignedNative().getLsFullwords().getLsPs9X9Low());
		assertEquals(2147483647, resp.getLsUnsignedNative().getLsFullwords().getLsPs9X9Max());
		assertEquals(-2147483648, resp.getLsUnsignedNative().getLsFullwords().getLsPs9X9Min());

		assertEquals(17179869183L, resp.getLsUnsignedNative().getLsDoublewords().getLsPs9X18High());
		assertEquals(-4294967294L, resp.getLsUnsignedNative().getLsDoublewords().getLsPs9X18Low());
		assertEquals(9223372036854775807L, resp.getLsUnsignedNative().getLsDoublewords().getLsPs9X18Max());
		assertEquals(-9223372036854775808L, resp.getLsUnsignedNative().getLsDoublewords().getLsPs9X18Min());
	}
}
