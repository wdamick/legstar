/*******************************************************************************
 * Copyright (c) 2009 LegSem.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 * 
 * Contributors:
 *     LegSem - initial API and implementation
 ******************************************************************************/
package com.legstar.mock.client;

import java.io.IOException;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.legstar.coxb.transform.HostTransformException;
import com.legstar.messaging.CommareaPart;
import com.legstar.messaging.HeaderPartException;
import com.legstar.messaging.LegStarMessage;
import com.legstar.messaging.RequestException;
import com.legstar.test.coxb.lsfileae.Dfhcommarea;
import com.legstar.test.coxb.lsfileal.RequestParms;
import com.legstar.test.coxb.lsfileal.bind.RequestParmsTransformers;
import com.legstar.test.coxb.lsfileal.bind.ReplyDataTransformers;
import com.legstar.test.coxb.lsfileal.Filler65;
import com.legstar.test.coxb.lsfileal.ReplyItem;
import com.legstar.test.coxb.lsfileal.ReplyPersonal;
import com.legstar.test.coxb.lsfileal.ReplyData;
import com.legstar.test.coxb.lsfileal.ReplySuccessHeader;

/**
 * Mocks the behavior of the LSFILEAE program.
 *
 */
public final class MockLsfileal {

    /** Logger. */
    private static final Log LOG = LogFactory.getLog(MockLsfileal.class);
    
    /** Utility class.*/
    private MockLsfileal() {
        
    }

    /**
     * Create a response to LSFILEAE execution request.
     * @param requestMessage the request message
     * @return formatted response
     * @throws RequestException if response cannot be built
     */
    public static  LegStarMessage getResponse(
            final LegStarMessage requestMessage) throws RequestException {
        if (LOG.isDebugEnabled()) {
            LOG.debug("Building response for program LSFILEAL");
        }
        try {
            RequestParmsTransformers requestParmsTransformers = new RequestParmsTransformers();
            RequestParms requestParms = requestParmsTransformers.toJava(
                    requestMessage.getDataParts().get(0).getContent());
            
            MockFILEA mockFILEA = new MockFILEA();
            String namePattern = requestParms.getRequestName();
            List < Dfhcommarea > customers = mockFILEA.getCustomers(
                    namePattern, mockFILEA.getCustomersList().size());
            
            ReplyData replyData = new ReplyData();
            ReplySuccessHeader replySuccessHeader = new ReplySuccessHeader();
            replySuccessHeader.setSearchDuration("00:00:00");
            replySuccessHeader.setTotalItemsRead(mockFILEA.getCustomersList().size());
            replyData.setReplySuccessHeader(replySuccessHeader);
            Filler65 replyFiller65 = new Filler65();
            for (Dfhcommarea dfhcommarea : customers) {
                ReplyItem replyItem = new ReplyItem();
                replyItem.setReplyNumber(dfhcommarea.getComNumber());
                ReplyPersonal replyPersonal = new ReplyPersonal();
                replyPersonal.setReplyName(dfhcommarea.getComPersonal().getComName());
                replyPersonal.setReplyAddress(dfhcommarea.getComPersonal().getComAddress());
                replyPersonal.setReplyPhone(dfhcommarea.getComPersonal().getComPhone());
                replyItem.setReplyPersonal(replyPersonal);
                replyItem.setReplyDate(dfhcommarea.getComDate());
                replyItem.setReplyAmount(dfhcommarea.getComAmount());
                replyItem.setReplyComment(dfhcommarea.getComComment());
                replyFiller65.getReplyItem().add(replyItem);
            }
            replyFiller65.setReplyItemscount(customers.size());
            replyData.setFiller65(replyFiller65);
            
            ReplyDataTransformers replyDataTransformers = new ReplyDataTransformers();
            LegStarMessage replyMessage = new LegStarMessage();
            replyMessage.addDataPart(new CommareaPart(
                    replyDataTransformers.toHost(replyData)));
            return replyMessage;
        } catch (HeaderPartException e) {
            throw new RequestException(e);
        } catch (HostTransformException e) {
            throw new RequestException(e);
        } catch (IOException e) {
            throw new RequestException(e);
        }
    }
    
}
