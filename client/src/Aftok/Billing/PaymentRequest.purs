module Aftok.Billing.PaymentRequest where

{--
<div role="document" class="ant-modal" style="width: 520px; transform-origin: 724px 147.062px;">
    <div tabindex="0" style="width: 0px; height: 0px; overflow: hidden;">sentinelStart</div>
    <div class="ant-modal-content"><button aria-label="Close" class="ant-modal-close"><span class="ant-modal-close-x"><i aria-label="icon: close" class="anticon anticon-close ant-modal-close-icon"><svg viewBox="64 64 896 896" class="" data-icon="close" width="1em" height="1em" fill="currentColor" aria-hidden="true"><path d="M563.8 512l262.5-312.9c4.4-5.2.7-13.1-6.1-13.1h-79.8c-4.7 0-9.2 2.1-12.3 5.7L511.6 449.8 295.1 191.7c-3-3.6-7.5-5.7-12.3-5.7H203c-6.8 0-10.5 7.9-6.1 13.1L459.4 512 196.9 824.9A7.95 7.95 0 0 0 203 838h79.8c4.7 0 9.2-2.1 12.3-5.7l216.5-258.1 216.5 258.1c3 3.6 7.5 5.7 12.3 5.7h79.8c6.8 0 10.5-7.9 6.1-13.1L563.8 512z"></path></svg></i></span></button>
        <div class="ant-modal-header">
            <div class="ant-modal-title" id="rcDialogTitle0">Tip a proposal</div>
        </div>
        <div class="ant-modal-body">
            <div class="TipJarModal">
                <div class="TipJarModal-uri">
                    <div>
                        <div class="TipJarModal-uri-qr"><span style="opacity: 1;"><canvas height="128" width="128" style="height: 128px; width: 128px;"></canvas></span></div>
                    </div>
                    <div class="TipJarModal-uri-info">
                        <div class="ant-row ant-form-item TipJarModal-uri-info-input CopyInput">
                            <div class="ant-form-item-label"><label class="" title="Amount">Amount</label></div>
                            <div class="ant-form-item-control-wrapper">
                                <div class="ant-form-item-control"><span class="ant-form-item-children"><span class="ant-input-group-wrapper"><span class="ant-input-wrapper ant-input-group"><input type="number" placeholder="Amount to send" class="ant-input" value="0.2"><span class="ant-input-group-addon">ZEC</span></span>
                                    </span>
                                    </span>
                                </div>
                            </div>
                        </div>
                        <div class="ant-row ant-form-item CopyInput TipJarModal-uri-info-input is-textarea">
                            <div class="ant-form-item-label"><label class="" title="Payment URI">Payment URI</label></div>
                            <div class="ant-form-item-control-wrapper">
                                <div class="ant-form-item-control"><span class="ant-form-item-children"><textarea readonly="" rows="3" class="ant-input">zcash:zs1xzymv205x8hhn8kt3pu43c6knjlelvxfgzgsyyus9yxhmdvqeu0yj0m2knzd3p93slsygkp94rz?amount=0.2</textarea><button type="button" class="ant-btn ant-btn-icon-only"><i aria-label="icon: copy" class="anticon anticon-copy"><svg viewBox="64 64 896 896" class="" data-icon="copy" width="1em" height="1em" fill="currentColor" aria-hidden="true"><path d="M832 64H296c-4.4 0-8 3.6-8 8v56c0 4.4 3.6 8 8 8h496v688c0 4.4 3.6 8 8 8h56c4.4 0 8-3.6 8-8V96c0-17.7-14.3-32-32-32zM704 192H192c-17.7 0-32 14.3-32 32v530.7c0 8.5 3.4 16.6 9.4 22.6l173.3 173.3c2.2 2.2 4.7 4 7.4 5.5v1.9h4.2c3.5 1.3 7.2 2 11 2H704c17.7 0 32-14.3 32-32V224c0-17.7-14.3-32-32-32zM350 856.2L263.9 770H350v86.2zM664 888H414V746c0-22.1-17.9-40-40-40H232V264h432v624z"></path></svg></i></button></span></div>
                            </div>
                        </div><a href="zcash:zs1xzymv205x8hhn8kt3pu43c6knjlelvxfgzgsyyus9yxhmdvqeu0yj0m2knzd3p93slsygkp94rz?amount=0.2" class="ant-btn ant-btn-ghost ant-btn-lg ant-btn-block"><span>Open in Wallet </span><i aria-label="icon: link" class="anticon anticon-link"><svg viewBox="64 64 896 896" class="" data-icon="link" width="1em" height="1em" fill="currentColor" aria-hidden="true"><path d="M574 665.4a8.03 8.03 0 0 0-11.3 0L446.5 781.6c-53.8 53.8-144.6 59.5-204 0-59.5-59.5-53.8-150.2 0-204l116.2-116.2c3.1-3.1 3.1-8.2 0-11.3l-39.8-39.8a8.03 8.03 0 0 0-11.3 0L191.4 526.5c-84.6 84.6-84.6 221.5 0 306s221.5 84.6 306 0l116.2-116.2c3.1-3.1 3.1-8.2 0-11.3L574 665.4zm258.6-474c-84.6-84.6-221.5-84.6-306 0L410.3 307.6a8.03 8.03 0 0 0 0 11.3l39.7 39.7c3.1 3.1 8.2 3.1 11.3 0l116.2-116.2c53.8-53.8 144.6-59.5 204 0 59.5 59.5 53.8 150.2 0 204L665.3 562.6a8.03 8.03 0 0 0 0 11.3l39.8 39.8c3.1 3.1 8.2 3.1 11.3 0l116.2-116.2c84.5-84.6 84.5-221.5 0-306.1zM610.1 372.3a8.03 8.03 0 0 0-11.3 0L372.3 598.7a8.03 8.03 0 0 0 0 11.3l39.6 39.6c3.1 3.1 8.2 3.1 11.3 0l226.4-226.4c3.1-3.1 3.1-8.2 0-11.3l-39.5-39.6z"></path></svg></i></a></div>
                </div>
                <div class="TipJarModal-fields">
                    <div class="TipJarModal-fields-row">
                        <div class="ant-row ant-form-item CopyInput TipJarModal-fields-row-address">
                            <div class="ant-form-item-label"><label class="" title="Address">Address</label></div>
                            <div class="ant-form-item-control-wrapper">
                                <div class="ant-form-item-control"><span class="ant-form-item-children"><input readonly="" type="text" class="ant-input" value="zs1xzymv205x8hhn8kt3pu43c6knjlelvxfgzgsyyus9yxhmdvqeu0yj0m2knzd3p93slsygkp94rz"><button type="button" class="ant-btn ant-btn-icon-only"><i aria-label="icon: copy" class="anticon anticon-copy"><svg viewBox="64 64 896 896" class="" data-icon="copy" width="1em" height="1em" fill="currentColor" aria-hidden="true"><path d="M832 64H296c-4.4 0-8 3.6-8 8v56c0 4.4 3.6 8 8 8h496v688c0 4.4 3.6 8 8 8h56c4.4 0 8-3.6 8-8V96c0-17.7-14.3-32-32-32zM704 192H192c-17.7 0-32 14.3-32 32v530.7c0 8.5 3.4 16.6 9.4 22.6l173.3 173.3c2.2 2.2 4.7 4 7.4 5.5v1.9h4.2c3.5 1.3 7.2 2 11 2H704c17.7 0 32-14.3 32-32V224c0-17.7-14.3-32-32-32zM350 856.2L263.9 770H350v86.2zM664 888H414V746c0-22.1-17.9-40-40-40H232V264h432v624z"></path></svg></i></button></span></div>
                            </div>
                        </div>
                    </div>
                    <div class="TipJarModal-fields-row">
                        <div class="ant-row ant-form-item ant-form-item-with-help CopyInput">
                            <div class="ant-form-item-label"><label class="" title="Zcash CLI command">Zcash CLI command</label></div>
                            <div class="ant-form-item-control-wrapper">
                                <div class="ant-form-item-control"><span class="ant-form-item-children"><input readonly="" type="text" class="ant-input" value="zcash-cli z_sendmany YOUR_ADDRESS '[{&quot;address&quot;:&quot;zs1xzymv205x8hhn8kt3pu43c6knjlelvxfgzgsyyus9yxhmdvqeu0yj0m2knzd3p93slsygkp94rz&quot;,&quot;amount&quot;:0.2}]'"><button type="button" class="ant-btn ant-btn-icon-only"><i aria-label="icon: copy" class="anticon anticon-copy"><svg viewBox="64 64 896 896" class="" data-icon="copy" width="1em" height="1em" fill="currentColor" aria-hidden="true"><path d="M832 64H296c-4.4 0-8 3.6-8 8v56c0 4.4 3.6 8 8 8h496v688c0 4.4 3.6 8 8 8h56c4.4 0 8-3.6 8-8V96c0-17.7-14.3-32-32-32zM704 192H192c-17.7 0-32 14.3-32 32v530.7c0 8.5 3.4 16.6 9.4 22.6l173.3 173.3c2.2 2.2 4.7 4 7.4 5.5v1.9h4.2c3.5 1.3 7.2 2 11 2H704c17.7 0 32-14.3 32-32V224c0-17.7-14.3-32-32-32zM350 856.2L263.9 770H350v86.2zM664 888H414V746c0-22.1-17.9-40-40-40H232V264h432v624z"></path></svg></i></button></span>
                                    <div class="ant-form-explain">Make sure you replace YOUR_ADDRESS with your actual address</div>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            </div>
        </div>
        <div class="ant-modal-footer"><button type="button" class="ant-btn ant-btn-primary"><span>Done</span></button></div>
    </div>
    <div tabindex="0" style="width: 0px; height: 0px; overflow: hidden;">sentinelEnd</div>
</div>

--}
